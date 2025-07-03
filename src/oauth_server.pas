{$mode objfpc}
{$codepage UTF8}
{$H+}

unit oauth_server;

interface

uses
  SysUtils, Classes, fphttpserver, httpdefs, httpprotocol, sockets, netdb
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TOAuthCallbackResult = record
    Success: Boolean;
    AuthorizationCode: AnsiString;
    State: AnsiString;
    ErrorMessage: AnsiString;
  end;

  // Forward declaration
  TOAuthServerThread = class;

  TOAuthServer = class
  private
    FServer: TFPHTTPServer;
    FPort: Word;
    FState: AnsiString;
    FCallbackResult: TOAuthCallbackResult;
    FCallbackReceived: Boolean;
    FServerThread: TOAuthServerThread;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function FindAvailablePort: Word;
  public
    constructor Create(const AExpectedState: AnsiString);
    destructor Destroy; override;
    function StartServer: Boolean;
    procedure StopServer;
    function WaitForCallback(TimeoutMs: Integer = 60000): TOAuthCallbackResult;
    function GetRedirectURI: AnsiString;
    property Port: Word read FPort;
    property RedirectURI: AnsiString read GetRedirectURI;
  end;

  // Thread class for running the HTTP server
  TOAuthServerThread = class(TThread)
  private
    FServer: TFPHTTPServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TFPHTTPServer);
    destructor Destroy; override;
    property Server: TFPHTTPServer read FServer;
  end;

const
  OAUTH_SUCCESS_URL = 'https://developers.google.com/gemini-code-assist/auth_success_gemini';
  OAUTH_FAILURE_URL = 'https://developers.google.com/gemini-code-assist/auth_failure_gemini';

implementation

// TOAuthServerThread implementation
constructor TOAuthServerThread.Create(AServer: TFPHTTPServer);
begin
  inherited Create(True); // Create suspended = True, don't start automatically
  FServer := AServer;
  FreeOnTerminate := False; // We'll manage the lifetime
end;

destructor TOAuthServerThread.Destroy;
begin
  if Assigned(FServer) then
  begin
    try
      FServer.Active := False;
    except
      on E: Exception do
        ; // Suppress exception during cleanup
    end;
  end;
  inherited Destroy;
end;

procedure TOAuthServerThread.Execute;
begin
  try
    FServer.Active := True;
    
    // The server.Active := True call will block here until the server is stopped
    // This is the expected behavior - the server runs in blocking mode
    
  except
    on E: Exception do
    begin
      // Suppress exception during execution
    end;
  end;
end;

constructor TOAuthServer.Create(const AExpectedState: AnsiString);
begin
  inherited Create;
  FState := AExpectedState;
  FCallbackReceived := False;
  FCallbackResult.Success := False;
  FCallbackResult.AuthorizationCode := AnsiString('');
  FCallbackResult.State := AnsiString('');
  FCallbackResult.ErrorMessage := AnsiString('');
  
  FServer := TFPHTTPServer.Create(nil);
  FServer.OnRequest := @HandleRequest;
  FServer.Threaded := False; // Keep it simple like the example
  FServerThread := nil;
end;

destructor TOAuthServer.Destroy;
var
  WaitCount: Integer;
  MaxWaitMs: Integer;
begin
  if Assigned(FServerThread) then
  begin
    // Stop the server first
    StopServer;
    
    FServerThread.Terminate;
    
    // Wait for the thread to finish, but with a reasonable timeout
    MaxWaitMs := 2000; // 2 seconds maximum wait
    WaitCount := 0;
    
    while not FServerThread.Finished and (WaitCount < MaxWaitMs div 100) do
    begin
      Sleep(100);
      Inc(WaitCount);
    end;
    
    if FServerThread.Finished then
    begin
      FServerThread.Free;
    end
    else
    begin
      // Don't free the thread - it's still running and freeing it would hang
      // The OS will clean it up when the process exits
    end;
  end;
  
  if Assigned(FServer) then
  begin
    FServer.Free;
  end;
  
  inherited Destroy;
end;

function TOAuthServer.FindAvailablePort: Word;
var
  Socket: TSocket;
  Addr: TInetSockAddr;
  AddrLen: TSockLen;
begin
  Result := 0;
  
  // Create a socket and bind to port 0 to get an available port
  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket = -1 then
  begin
    Exit;
  end;
    
  try
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := htonl($7F000001); // 127.0.0.1 (INADDR_LOOPBACK)
    Addr.sin_port := 0; // Let system choose port
    
    if fpBind(Socket, @Addr, SizeOf(Addr)) = 0 then
    begin
      AddrLen := SizeOf(Addr);
      if fpGetSockName(Socket, @Addr, @AddrLen) = 0 then
      begin
        Result := ntohs(Addr.sin_port);
      end;
    end;
  finally
    CloseSocket(Socket);
  end;
end;

function TOAuthServer.StartServer: Boolean;
begin
  Result := False;
  
  // Find an available port
  FPort := FindAvailablePort;
  if FPort = 0 then
  begin
    Exit;
  end;
    
  try
    FServer.Port := FPort;
    FServer.AcceptIdleTimeout := 100; // 100ms timeout
    
    // Start server in a separate thread
    FServerThread := TOAuthServerThread.Create(FServer);
    
    // Start the thread and give it a moment to activate the server
    FServerThread.Start;
    Sleep(500);
    
    Result := True;
  except
    on E: Exception do
    begin
      FCallbackResult.ErrorMessage := AnsiString('Failed to start server: ' + E.Message);
      Result := False;
    end;
  end;
end;


procedure TOAuthServer.StopServer;
begin
  if Assigned(FServerThread) and Assigned(FServer) then
  begin
    // Stop the server through the thread's property
    if FServerThread.Server.Active then
    begin
      FServerThread.Server.Active := False;
    end;
  end;
end;

function TOAuthServer.GetRedirectURI: AnsiString;
begin
  Result := AnsiString('http://localhost:' + IntToStr(FPort) + '/oauth2callback');
end;

procedure TOAuthServer.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  QueryParams: TStringList;
  I: Integer;
  ParamName, ParamValue: AnsiString;
  ReceivedState: AnsiString;
  AuthCode: AnsiString;
  ErrorParam: AnsiString;
begin
  // Only handle the OAuth callback path
  if ARequest.PathInfo <> '/oauth2callback' then
  begin
    AResponse.Code := 404;
    AResponse.CodeText := 'Not Found';
    AResponse.Content := 'Invalid request path';
    Exit;
  end;
  
  QueryParams := TStringList.Create;
  try
    // Parse query parameters
    QueryParams.Delimiter := '&';
    QueryParams.StrictDelimiter := True;
    QueryParams.DelimitedText := ARequest.QueryString;
    
    ReceivedState := AnsiString('');
    AuthCode := AnsiString('');
    ErrorParam := AnsiString('');
    
    // Extract relevant parameters
    for I := 0 to QueryParams.Count - 1 do
    begin
      ParamName := AnsiString(QueryParams.Names[I]);
      ParamValue := AnsiString(QueryParams.ValueFromIndex[I]);
      
      if ParamName = 'state' then
        ReceivedState := ParamValue
      else if ParamName = 'code' then
        AuthCode := ParamValue
      else if ParamName = 'error' then
        ErrorParam := ParamValue;
    end;
    
    // Validate and process the callback
    if ErrorParam <> '' then
    begin
      FCallbackResult.Success := False;
      FCallbackResult.ErrorMessage := AnsiString('OAuth error: ' + string(ErrorParam));
      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', OAUTH_FAILURE_URL);
    end
    else if ReceivedState <> FState then
    begin
      FCallbackResult.Success := False;
      FCallbackResult.ErrorMessage := AnsiString('State mismatch. Possible CSRF attack');
      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', OAUTH_FAILURE_URL);
    end
    else if AuthCode = '' then
    begin
      FCallbackResult.Success := False;
      FCallbackResult.ErrorMessage := AnsiString('No authorization code received');
      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', OAUTH_FAILURE_URL);
    end
    else
    begin
      // Success!
      FCallbackResult.Success := True;
      FCallbackResult.AuthorizationCode := AuthCode;
      FCallbackResult.State := ReceivedState;
      FCallbackResult.ErrorMessage := AnsiString('');
      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', OAUTH_SUCCESS_URL);
    end;
    
    FCallbackReceived := True;
    
  finally
    QueryParams.Free;
  end;
end;

function TOAuthServer.WaitForCallback(TimeoutMs: Integer): TOAuthCallbackResult;
var
  StartTime: QWord;
  ElapsedMs: QWord;
begin
  StartTime := GetTickCount64;
  FCallbackReceived := False;
  
  while not FCallbackReceived do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs > TimeoutMs then
    begin
      FCallbackResult.Success := False;
      FCallbackResult.ErrorMessage := AnsiString('Timeout waiting for OAuth callback');
      Break;
    end;
    
    // Allow the server to process requests
    try
      if FServer.Active then
      begin
        // Process any pending requests
        // The server should handle this automatically
        Sleep(100);
      end
      else
      begin
        FCallbackResult.Success := False;
        FCallbackResult.ErrorMessage := AnsiString('Server stopped unexpectedly');
        Break;
      end;
    except
      on E: Exception do
      begin
        FCallbackResult.Success := False;
        FCallbackResult.ErrorMessage := AnsiString('Server error: ' + E.Message);
        Break;
      end;
    end;
  end;
  
  Result := FCallbackResult;
end;

end.