{$mode objfpc}
{$codepage UTF8}
{$H+}

unit claude_oauth_client;

interface

uses
  SysUtils, Classes, DateUtils, fphttpclient, opensslsockets, fpjson, jsonparser,
  oauth_client, base64, Process, logging;

type
  TClaudeOAuthClient = class(TOAuthClient)
  private
    FCodeVerifier: AnsiString;
    FCodeChallenge: AnsiString;
    function GenerateCodeVerifier: AnsiString;
    function GenerateCodeChallenge(const Verifier: AnsiString): AnsiString;
    function GenerateRandomUrlSafeBase64(ByteCount: Integer): AnsiString;
  public
    constructor Create;
    function BuildAuthorizationURL(const RedirectURI, State: AnsiString): AnsiString; override;
    function ExchangeCodeForTokens(const AuthCode, RedirectURI: AnsiString): TOAuthTokens; override;
    function RefreshTokens(const RefreshToken: AnsiString): TOAuthTokens; override;
    function ValidateToken(const AccessToken: AnsiString): Boolean; override;
  end;

implementation

constructor TClaudeOAuthClient.Create;
begin
  inherited Create;
  // Generate PKCE parameters
  FCodeVerifier := GenerateCodeVerifier;
  FCodeChallenge := GenerateCodeChallenge(FCodeVerifier);
end;

function TClaudeOAuthClient.GenerateRandomUrlSafeBase64(ByteCount: Integer): AnsiString;
var
  I: Integer;
  RandomByte: Byte;
  BinaryData: AnsiString;
begin
  BinaryData := AnsiString('');
  Randomize;
  
  for I := 0 to ByteCount - 1 do
  begin
    RandomByte := Random(256);
    BinaryData := BinaryData + AnsiChar(RandomByte);
  end;
  
  // Convert to base64 and make URL-safe
  Result := AnsiString(EncodeStringBase64(string(BinaryData)));
  // Make URL-safe by replacing characters
  Result := StringReplace(string(Result), '+', '-', [rfReplaceAll]);
  Result := StringReplace(string(Result), '/', '_', [rfReplaceAll]);
  Result := StringReplace(string(Result), '=', '', [rfReplaceAll]);
end;

function TClaudeOAuthClient.GenerateCodeVerifier: AnsiString;
begin
  // Generate 32 random bytes and encode as URL-safe base64
  // This gives us a 43-character string (32 bytes * 4/3 = 42.67, rounded up)
  Result := GenerateRandomUrlSafeBase64(32);
end;

function TClaudeOAuthClient.GenerateCodeChallenge(const Verifier: AnsiString): AnsiString;
var
  Process: TProcess;
  TempFile: AnsiString;
  BinaryHash: AnsiString;
  OutputBytes: TBytes;
  F: TFileStream;
  I: Integer;
begin
  // Use system openssl command to generate SHA256 hash
  Process := TProcess.Create(nil);
  try
    // Create temporary file for input
    TempFile := AnsiString(GetTempFileName('', 'oauth_'));
    F := TFileStream.Create(string(TempFile), fmCreate);
    try
      F.Write(Verifier[1], Length(Verifier));
    finally
      F.Free;
    end;
    
    Process.Executable := 'openssl';
    Process.Parameters.Add('dgst');
    Process.Parameters.Add('-sha256');
    Process.Parameters.Add('-binary');
    Process.Parameters.Add(string(TempFile));
    Process.Options := [poUsePipes, poWaitOnExit];
    
    try
      Process.Execute;
      Process.WaitOnExit;
      
      if Process.ExitStatus = 0 then
      begin
        // Read binary output using a stream
        SetLength(OutputBytes, 32); // SHA256 produces exactly 32 bytes
        if Process.Output.Read(OutputBytes[0], 32) = 32 then
        begin
          // Convert to AnsiString
          BinaryHash := AnsiString('');
          for I := 0 to High(OutputBytes) do
            BinaryHash := BinaryHash + AnsiChar(OutputBytes[I]);
          
          // Base64 encode and make URL-safe
          Result := AnsiString(EncodeStringBase64(string(BinaryHash)));
          Result := StringReplace(string(Result), '+', '-', [rfReplaceAll]);
          Result := StringReplace(string(Result), '/', '_', [rfReplaceAll]);
          Result := StringReplace(string(Result), '=', '', [rfReplaceAll]);
        end
        else
        begin
          WriteLn('Warning: OpenSSL SHA256 output size mismatch, falling back to plain PKCE');
          Result := Verifier;
        end;
      end
      else
      begin
        // Fallback to plain method if openssl fails
        WriteLn('Warning: OpenSSL SHA256 failed, falling back to plain PKCE');
        Result := Verifier;
      end;
    except
      on E: Exception do
      begin
        WriteLn('Warning: OpenSSL execution failed: ', E.Message);
        Result := Verifier;
      end;
    end;
    
    // Clean up temp file
    try
      DeleteFile(string(TempFile));
    except
      // Ignore cleanup errors
    end;
  finally
    Process.Free;
  end;
end;

function TClaudeOAuthClient.BuildAuthorizationURL(const RedirectURI, State: AnsiString): AnsiString;
begin
  Result := AnsiString(CLAUDE_OAUTH_AUTH_ENDPOINT) + 
    AnsiString('?response_type=code') +
    AnsiString('&client_id=') + AnsiString(CLAUDE_OAUTH_CLIENT_ID) +
    AnsiString('&redirect_uri=') + RedirectURI +
    AnsiString('&scope=') + AnsiString(CLAUDE_OAUTH_SCOPES) +
    AnsiString('&state=') + State +
    AnsiString('&code_challenge=') + FCodeChallenge +
    AnsiString('&code_challenge_method=S256');
end;

function TClaudeOAuthClient.ExchangeCodeForTokens(const AuthCode, RedirectURI: AnsiString): TOAuthTokens;
var
  RequestJson: TJSONObject;
  Response: string;
  JsonParser: TJSONParser;
  JsonData: TJSONObject;
  JsonValue: TJSONData;
  ExpiresIn: Integer;
  RequestBody: TStringStream;
  CodePart, StatePart: string;
  HashPos: Integer;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := AnsiString('');
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  // Split authorization code like opencode does: code#state
  HashPos := Pos('#', string(AuthCode));
  if HashPos > 0 then
  begin
    CodePart := Copy(string(AuthCode), 1, HashPos - 1);
    StatePart := Copy(string(AuthCode), HashPos + 1, Length(string(AuthCode)));
  end
  else
  begin
    CodePart := string(AuthCode);
    StatePart := '';
  end;
  
  // Create JSON request body (like opencode does)
  RequestJson := TJSONObject.Create;
  try
    RequestJson.Add('grant_type', 'authorization_code');
    RequestJson.Add('code', CodePart);
    RequestJson.Add('state', StatePart);
    RequestJson.Add('redirect_uri', string(RedirectURI));
    RequestJson.Add('client_id', CLAUDE_OAUTH_CLIENT_ID);
    RequestJson.Add('code_verifier', string(FCodeVerifier));
    
    RequestBody := TStringStream.Create(RequestJson.AsJSON);
    try
      FHttpClient.RequestBody := RequestBody;
      FHttpClient.AddHeader('Content-Type', 'application/json');
      Response := FHttpClient.Post(CLAUDE_OAUTH_TOKEN_ENDPOINT);
      
      if FHttpClient.ResponseStatusCode <> 200 then
        raise Exception.Create('Token exchange failed: HTTP ' + IntToStr(FHttpClient.ResponseStatusCode) + ' - ' + Response);
      
      // Parse JSON response
      JsonParser := TJSONParser.Create(Response);
      try
        JsonData := JsonParser.Parse as TJSONObject;
        try
          // Extract access token
          JsonValue := JsonData.Find('access_token');
          if Assigned(JsonValue) then
            Result.AccessToken := AnsiString(JsonValue.AsString);
          
          // Extract refresh token
          JsonValue := JsonData.Find('refresh_token');
          if Assigned(JsonValue) then
            Result.RefreshToken := AnsiString(JsonValue.AsString);
          
          // Extract token type
          JsonValue := JsonData.Find('token_type');
          if Assigned(JsonValue) then
            Result.TokenType := AnsiString(JsonValue.AsString);
          
          // Extract expires_in
          JsonValue := JsonData.Find('expires_in');
          if Assigned(JsonValue) then
          begin
            ExpiresIn := JsonValue.AsInteger;
            Result.ExpiresIn := ExpiresIn;
            Result.ExpiresAt := DateTimeToUnix(Now) + ExpiresIn;
          end;
          
          // Extract scope
          JsonValue := JsonData.Find('scope');
          if Assigned(JsonValue) then
            Result.Scope := AnsiString(JsonValue.AsString);
          
          if Result.AccessToken = '' then
            raise Exception.Create('No access token received from Claude OAuth service');
            
        finally
          JsonData.Free;
        end;
      finally
        JsonParser.Free;
      end;
    finally
      RequestBody.Free;
      FHttpClient.RequestBody := nil;
    end;
  finally
    RequestJson.Free;
  end;
end;

function TClaudeOAuthClient.RefreshTokens(const RefreshToken: AnsiString): TOAuthTokens;
var
  RequestJson: TJSONObject;
  JsonString: string;
  Response: string;
  JsonParser: TJSONParser;
  JsonData: TJSONObject;
  JsonValue: TJSONData;
  ExpiresIn: Integer;
  i: Integer;
  ActualBodyBytes: array of Byte;
  ActualBodyString: string;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := RefreshToken; // Keep the same refresh token
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  RequestJson := TJSONObject.Create;
  try
    RequestJson.Add('grant_type', 'refresh_token');
    RequestJson.Add('refresh_token', string(RefreshToken));
    RequestJson.Add('client_id', CLAUDE_OAUTH_CLIENT_ID);
    
    JsonString := RequestJson.AsJSON;
    
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Request Details:');
    DebugLog('[ClaudeOAuthClient.RefreshTokens] URL: ' + CLAUDE_OAUTH_TOKEN_ENDPOINT);
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Method: POST');
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Content-Type: application/json');
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Client ID: ' + CLAUDE_OAUTH_CLIENT_ID);
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Refresh Token (first 20 chars): ' + Copy(string(RefreshToken), 1, 20) + '...');
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Request Body: ' + JsonString);
    DebugLog('[ClaudeOAuthClient.RefreshTokens] Request Body Length: ' + IntToStr(Length(JsonString)));
    
    FHttpClient.RequestBody := TStringStream.Create(JsonString);
    try
      FHttpClient.AddHeader('Content-Type', 'application/json');
      
      // Log the ACTUAL headers that will be sent
      DebugLog('[ClaudeOAuthClient.RefreshTokens] ACTUAL Request Headers:');
      if Assigned(FHttpClient.RequestHeaders) then
      begin
        for i := 0 to FHttpClient.RequestHeaders.Count - 1 do
          DebugLog('[ClaudeOAuthClient.RefreshTokens] Header[' + IntToStr(i) + ']: ' + FHttpClient.RequestHeaders[i]);
      end
      else
        DebugLog('[ClaudeOAuthClient.RefreshTokens] No request headers found');
      
      // Log the ACTUAL request body
      if Assigned(FHttpClient.RequestBody) then
      begin
        FHttpClient.RequestBody.Position := 0;
        SetLength(ActualBodyBytes, FHttpClient.RequestBody.Size);
        FHttpClient.RequestBody.ReadBuffer(ActualBodyBytes[0], FHttpClient.RequestBody.Size);
        FHttpClient.RequestBody.Position := 0; // Reset position for the actual request
        ActualBodyString := '';
        for i := 0 to High(ActualBodyBytes) do
          ActualBodyString := ActualBodyString + Chr(ActualBodyBytes[i]);
        DebugLog('[ClaudeOAuthClient.RefreshTokens] ACTUAL Request Body (JSON): ' + ActualBodyString);
        DebugLog('[ClaudeOAuthClient.RefreshTokens] ACTUAL Request Body Length: ' + IntToStr(Length(ActualBodyString)));
      end
      else
        DebugLog('[ClaudeOAuthClient.RefreshTokens] No request body found');
      
      DebugLog('[ClaudeOAuthClient.RefreshTokens] Making HTTP request...');
      Response := FHttpClient.Post(CLAUDE_OAUTH_TOKEN_ENDPOINT);
      
      // Log ACTUAL response headers
      DebugLog('[ClaudeOAuthClient.RefreshTokens] ACTUAL Response Headers:');
      if Assigned(FHttpClient.ResponseHeaders) then
      begin
        for i := 0 to FHttpClient.ResponseHeaders.Count - 1 do
          DebugLog('[ClaudeOAuthClient.RefreshTokens] Response Header[' + IntToStr(i) + ']: ' + FHttpClient.ResponseHeaders[i]);
      end
      else
        DebugLog('[ClaudeOAuthClient.RefreshTokens] No response headers found');
      
      DebugLog('[ClaudeOAuthClient.RefreshTokens] Response Status: ' + IntToStr(FHttpClient.ResponseStatusCode));
      DebugLog('[ClaudeOAuthClient.RefreshTokens] Response Body: ' + Response);
      DebugLog('[ClaudeOAuthClient.RefreshTokens] Response Body Length: ' + IntToStr(Length(Response)));
      
      if FHttpClient.ResponseStatusCode <> 200 then
        raise Exception.Create('Token refresh failed: HTTP ' + IntToStr(FHttpClient.ResponseStatusCode) + ' - ' + Response);
      
      // Parse JSON response
      JsonParser := TJSONParser.Create(Response);
      try
        JsonData := JsonParser.Parse as TJSONObject;
        try
          // Extract access token
          JsonValue := JsonData.Find('access_token');
          if Assigned(JsonValue) then
            Result.AccessToken := AnsiString(JsonValue.AsString);
          
          // Extract new refresh token (if provided)
          JsonValue := JsonData.Find('refresh_token');
          if Assigned(JsonValue) then
            Result.RefreshToken := AnsiString(JsonValue.AsString);
          
          // Extract token type
          JsonValue := JsonData.Find('token_type');
          if Assigned(JsonValue) then
            Result.TokenType := AnsiString(JsonValue.AsString);
          
          // Extract expires_in
          JsonValue := JsonData.Find('expires_in');
          if Assigned(JsonValue) then
          begin
            ExpiresIn := JsonValue.AsInteger;
            Result.ExpiresIn := ExpiresIn;
            Result.ExpiresAt := DateTimeToUnix(Now) + ExpiresIn;
          end;
          
          // Extract scope
          JsonValue := JsonData.Find('scope');
          if Assigned(JsonValue) then
            Result.Scope := AnsiString(JsonValue.AsString);
          
          if Result.AccessToken = '' then
            raise Exception.Create('No access token received from Claude token refresh');
            
        finally
          JsonData.Free;
        end;
      finally
        JsonParser.Free;
      end;
    finally
      FHttpClient.RequestBody.Free;
      FHttpClient.RequestBody := nil;
    end;
  finally
    RequestJson.Free;
  end;
end;

function TClaudeOAuthClient.ValidateToken(const AccessToken: AnsiString): Boolean;
begin
  // Claude doesn't have a public token validation endpoint like Google
  // For now, we'll assume the token is valid if it's not empty
  // In a production implementation, we might try a simple API call to validate
  Result := AccessToken <> '';
end;

end.