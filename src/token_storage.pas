{$mode objfpc}
{$codepage UTF8}
{$H+}

unit token_storage;

interface

uses
  SysUtils, Classes, fpjson, jsonparser, oauth_client;

type
  TTokenStorage = class
  private
    function GetConfigDir: AnsiString;
    function GetTokenFilePath(const Provider: AnsiString = ''): AnsiString;
    function EnsureConfigDir: Boolean;
    function SetFilePermissions(const FilePath: AnsiString): Boolean;
  public
    // Legacy methods (for backward compatibility with Gemini)
    function SaveTokens(const Tokens: TOAuthTokens): Boolean;
    function LoadTokens: TOAuthTokens;
    function HasValidTokens: Boolean;
    function ClearTokens: Boolean;
    function TokensExist: Boolean;
    
    // Provider-specific methods
    function SaveProviderTokens(const Provider: AnsiString; const Tokens: TOAuthTokens): Boolean;
    function LoadProviderTokens(const Provider: AnsiString): TOAuthTokens;
    function HasValidProviderTokens(const Provider: AnsiString): Boolean;
    function ClearProviderTokens(const Provider: AnsiString): Boolean;
    function ProviderTokensExist(const Provider: AnsiString): Boolean;
  end;

const
  GITPAL_CONFIG_DIR = 'gitpal';
  OAUTH_CREDS_FILE = 'oauth_creds.json';

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  DateUtils;

function TTokenStorage.GetConfigDir: AnsiString;
var
  XDGConfigHome: AnsiString;
  HomeDir: AnsiString;
begin
  // Try XDG_CONFIG_HOME first
  XDGConfigHome := AnsiString(GetEnvironmentVariable('XDG_CONFIG_HOME'));
  if XDGConfigHome <> '' then
  begin
    Result := XDGConfigHome + DirectorySeparator + GITPAL_CONFIG_DIR;
    Exit;
  end;
  
  // Fallback to ~/.config/gitpal
  HomeDir := AnsiString(GetEnvironmentVariable('HOME'));
  if HomeDir <> '' then
  begin
    Result := HomeDir + DirectorySeparator + '.config' + DirectorySeparator + GITPAL_CONFIG_DIR;
    Exit;
  end;
  
  // Last resort: current directory
  Result := AnsiString(GetCurrentDir) + DirectorySeparator + GITPAL_CONFIG_DIR;
end;

function TTokenStorage.GetTokenFilePath(const Provider: AnsiString): AnsiString;
begin
  if Provider = '' then
    // Legacy path for Gemini compatibility
    Result := GetConfigDir + DirectorySeparator + OAUTH_CREDS_FILE
  else
    // Provider-specific path
    Result := GetConfigDir + DirectorySeparator + 'oauth_creds_' + Provider + '.json';
end;

function TTokenStorage.EnsureConfigDir: Boolean;
var
  ConfigDir: AnsiString;
begin
  ConfigDir := GetConfigDir;
  
  try
    if not DirectoryExists(string(ConfigDir)) then
    begin
      if not ForceDirectories(string(ConfigDir)) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TTokenStorage.SetFilePermissions(const FilePath: AnsiString): Boolean;
{$IFDEF UNIX}
var
  Mode: TMode;
{$ENDIF}
begin
  Result := True;
  
  {$IFDEF UNIX}
  try
    // Set permissions to 0600 (owner read/write only)
    Mode := S_IRUSR or S_IWUSR;
    if fpChmod(PChar(FilePath), Mode) <> 0 then
      Result := False;
  except
    Result := False;
  end;
  {$ENDIF}
  
  // On Windows, we don't set special permissions
  // The file will inherit default user permissions
end;

function TTokenStorage.SaveTokens(const Tokens: TOAuthTokens): Boolean;
var
  JsonObject: TJSONObject;
  JsonString: AnsiString;
  FileStream: TFileStream;
  TokenFilePath: AnsiString;
begin
  Result := False;
  
  try
    // Ensure config directory exists
    if not EnsureConfigDir then
      Exit;
    
    // Create JSON object with token data
    JsonObject := TJSONObject.Create;
    try
      JsonObject.Add('access_token', string(Tokens.AccessToken));
      JsonObject.Add('refresh_token', string(Tokens.RefreshToken));
      JsonObject.Add('token_type', string(Tokens.TokenType));
      JsonObject.Add('expires_in', Tokens.ExpiresIn);
      JsonObject.Add('expires_at', Tokens.ExpiresAt);
      JsonObject.Add('scope', string(Tokens.Scope));
      
      // Convert to JSON string
      JsonString := AnsiString(JsonObject.AsJSON);
      
      // Write to file
      TokenFilePath := GetTokenFilePath;
      FileStream := TFileStream.Create(string(TokenFilePath), fmCreate);
      try
        FileStream.WriteBuffer(JsonString[1], Length(JsonString));
        Result := True;
      finally
        FileStream.Free;
      end;
      
      // Set secure file permissions
      if Result then
        SetFilePermissions(TokenFilePath);
        
    finally
      JsonObject.Free;
    end;
    
  except
    on E: Exception do
    begin
      Result := False;
      // Could log error here if needed
    end;
  end;
end;

function TTokenStorage.LoadTokens: TOAuthTokens;
var
  TokenFilePath: AnsiString;
  FileStream: TFileStream;
  JsonString: AnsiString;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
  FileSize: Int64;
begin
  // Initialize result with empty values
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := AnsiString('');
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  try
    TokenFilePath := GetTokenFilePath;
    
    if not FileExists(string(TokenFilePath)) then
      Exit;
    
    // Read file contents
    FileStream := TFileStream.Create(string(TokenFilePath), fmOpenRead);
    try
      FileSize := FileStream.Size;
      if FileSize = 0 then
        Exit;
        
      SetLength(JsonString, FileSize);
      FileStream.ReadBuffer(JsonString[1], FileSize);
    finally
      FileStream.Free;
    end;
    
    // Parse JSON
    JsonData := GetJSON(string(JsonString));
    try
      if JsonData is TJSONObject then
      begin
        JsonObject := TJSONObject(JsonData);
        
        // Extract token data
        if JsonObject.Find('access_token') <> nil then
          Result.AccessToken := AnsiString(JsonObject.Get('access_token', ''));
          
        if JsonObject.Find('refresh_token') <> nil then
          Result.RefreshToken := AnsiString(JsonObject.Get('refresh_token', ''));
          
        if JsonObject.Find('token_type') <> nil then
          Result.TokenType := AnsiString(JsonObject.Get('token_type', ''));
          
        if JsonObject.Find('expires_in') <> nil then
          Result.ExpiresIn := JsonObject.Get('expires_in', 0);
          
        if JsonObject.Find('expires_at') <> nil then
          Result.ExpiresAt := JsonObject.Get('expires_at', 0);
          
        if JsonObject.Find('scope') <> nil then
          Result.Scope := AnsiString(JsonObject.Get('scope', ''));
      end;
    finally
      JsonData.Free;
    end;
    
  except
    // If any error occurs, return empty tokens
    Result.AccessToken := AnsiString('');
    Result.RefreshToken := AnsiString('');
    Result.TokenType := AnsiString('');
    Result.ExpiresIn := 0;
    Result.ExpiresAt := 0;
    Result.Scope := AnsiString('');
  end;
end;

function TTokenStorage.HasValidTokens: Boolean;
var
  Tokens: TOAuthTokens;
  CurrentTime: Int64;
begin
  Result := False;
  
  Tokens := LoadTokens;
  
  // Check if we have an access token
  if Tokens.AccessToken = '' then
    Exit;
  
  // Check if token is not expired (with 5 minute buffer)
  CurrentTime := DateTimeToUnix(Now);
  if (Tokens.ExpiresAt > 0) and (Tokens.ExpiresAt <= CurrentTime + 300) then
  begin
    // Token is expired or about to expire
    // We could try to refresh it here, but for now just return false
    Exit;
  end;
  
  Result := True;
end;

function TTokenStorage.TokensExist: Boolean;
var
  TokenFilePath: AnsiString;
begin
  TokenFilePath := GetTokenFilePath;
  Result := FileExists(string(TokenFilePath));
end;

function TTokenStorage.ClearTokens: Boolean;
var
  TokenFilePath: AnsiString;
begin
  Result := True;
  
  try
    TokenFilePath := GetTokenFilePath;
    
    if FileExists(string(TokenFilePath)) then
    begin
      if not DeleteFile(string(TokenFilePath)) then
        Result := False;
    end;
  except
    Result := False;
  end;
end;

// Provider-specific method implementations

function TTokenStorage.SaveProviderTokens(const Provider: AnsiString; const Tokens: TOAuthTokens): Boolean;
var
  JsonObj: TJSONObject;
  JsonString: AnsiString;
  FileStream: TFileStream;
  TokenFilePath: AnsiString;
begin
  Result := False;
  
  if not EnsureConfigDir then
    Exit;
  
  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('access_token', string(Tokens.AccessToken));
    JsonObj.Add('refresh_token', string(Tokens.RefreshToken));
    JsonObj.Add('token_type', string(Tokens.TokenType));
    JsonObj.Add('expires_in', Tokens.ExpiresIn);
    JsonObj.Add('expires_at', Tokens.ExpiresAt);
    JsonObj.Add('scope', string(Tokens.Scope));
    
    JsonString := AnsiString(JsonObj.AsJSON);
    TokenFilePath := GetTokenFilePath(Provider);
    
    try
      FileStream := TFileStream.Create(string(TokenFilePath), fmCreate);
      try
        FileStream.WriteBuffer(JsonString[1], Length(JsonString));
        Result := SetFilePermissions(TokenFilePath);
      finally
        FileStream.Free;
      end;
    except
      Result := False;
    end;
  finally
    JsonObj.Free;
  end;
end;

function TTokenStorage.LoadProviderTokens(const Provider: AnsiString): TOAuthTokens;
var
  JsonParser: TJSONParser;
  JsonData: TJSONObject;
  JsonValue: TJSONData;
  FileContent: string;
  TokenFilePath: AnsiString;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := AnsiString('');
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  TokenFilePath := GetTokenFilePath(Provider);
  
  if not FileExists(string(TokenFilePath)) then
    Exit;
  
  try
    with TFileStream.Create(string(TokenFilePath), fmOpenRead) do
    try
      SetLength(FileContent, Size);
      ReadBuffer(FileContent[1], Size);
    finally
      Free;
    end;
    
    JsonParser := TJSONParser.Create(FileContent);
    try
      JsonData := JsonParser.Parse as TJSONObject;
      try
        JsonValue := JsonData.Find('access_token');
        if Assigned(JsonValue) then
          Result.AccessToken := AnsiString(JsonValue.AsString);
        
        JsonValue := JsonData.Find('refresh_token');
        if Assigned(JsonValue) then
          Result.RefreshToken := AnsiString(JsonValue.AsString);
        
        JsonValue := JsonData.Find('token_type');
        if Assigned(JsonValue) then
          Result.TokenType := AnsiString(JsonValue.AsString);
        
        JsonValue := JsonData.Find('expires_in');
        if Assigned(JsonValue) then
          Result.ExpiresIn := JsonValue.AsInteger;
        
        JsonValue := JsonData.Find('expires_at');
        if Assigned(JsonValue) then
          Result.ExpiresAt := JsonValue.AsInt64;
        
        JsonValue := JsonData.Find('scope');
        if Assigned(JsonValue) then
          Result.Scope := AnsiString(JsonValue.AsString);
          
      finally
        JsonData.Free;
      end;
    finally
      JsonParser.Free;
    end;
  except
    // If anything goes wrong, return empty tokens
    FillChar(Result, SizeOf(Result), 0);
    Result.AccessToken := AnsiString('');
    Result.RefreshToken := AnsiString('');
    Result.TokenType := AnsiString('');
    Result.ExpiresIn := 0;
    Result.ExpiresAt := 0;
    Result.Scope := AnsiString('');
  end;
end;

function TTokenStorage.HasValidProviderTokens(const Provider: AnsiString): Boolean;
var
  Tokens: TOAuthTokens;
  CurrentTime: Int64;
  BufferTime: Int64;
begin
  Result := False;
  
  if not ProviderTokensExist(Provider) then
    Exit;
  
  Tokens := LoadProviderTokens(Provider);
  
  if Tokens.AccessToken = '' then
    Exit;
  
  // Check if token is expired (with 5-minute buffer)
  CurrentTime := DateTimeToUnix(Now);
  BufferTime := 5 * 60; // 5 minutes in seconds
  
  Result := (Tokens.ExpiresAt > 0) and (Tokens.ExpiresAt > (CurrentTime + BufferTime));
end;

function TTokenStorage.ClearProviderTokens(const Provider: AnsiString): Boolean;
var
  TokenFilePath: AnsiString;
begin
  Result := True;
  TokenFilePath := GetTokenFilePath(Provider);
  
  try
    if FileExists(string(TokenFilePath)) then
    begin
      if not DeleteFile(string(TokenFilePath)) then
        Result := False;
    end;
  except
    Result := False;
  end;
end;

function TTokenStorage.ProviderTokensExist(const Provider: AnsiString): Boolean;
var
  TokenFilePath: AnsiString;
begin
  TokenFilePath := GetTokenFilePath(Provider);
  Result := FileExists(string(TokenFilePath));
end;

end.