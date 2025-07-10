{$mode objfpc}
{$codepage UTF8}
{$H+}

unit oauth_client;

interface

uses
  SysUtils, Classes, DateUtils, fphttpclient, opensslsockets, fpjson, jsonparser;

type
  TOAuthTokens = record
    AccessToken: AnsiString;
    RefreshToken: AnsiString;
    TokenType: AnsiString;
    ExpiresIn: Integer;
    ExpiresAt: Int64; // Unix timestamp
    Scope: AnsiString;
  end;

  TOAuthClient = class
  private
    function GenerateRandomHex(ByteCount: Integer): AnsiString;
  protected
    FHttpClient: TFPHTTPClient;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateState: AnsiString;
    function BuildAuthorizationURL(const RedirectURI, State: AnsiString): AnsiString; virtual;
    function ExchangeCodeForTokens(const AuthCode, RedirectURI: AnsiString): TOAuthTokens; virtual;
    function RefreshTokens(const RefreshToken: AnsiString): TOAuthTokens; virtual;
    function ValidateToken(const AccessToken: AnsiString): Boolean; virtual;
  end;

const
  // Google OAuth 2.0 configuration (from gemini-cli)
  GOOGLE_OAUTH_CLIENT_ID = '681255809395-oo8ft2oprdrnp9e3aqf6av3hmdib135j.apps.googleusercontent.com';
  GOOGLE_OAUTH_CLIENT_SECRET = ''; // Public client - no secret needed for PKCE
  GOOGLE_OAUTH_AUTH_ENDPOINT = 'https://accounts.google.com/o/oauth2/v2/auth';
  GOOGLE_OAUTH_TOKEN_ENDPOINT = 'https://oauth2.googleapis.com/token';
  GOOGLE_OAUTH_TOKEN_INFO_ENDPOINT = 'https://oauth2.googleapis.com/tokeninfo';
  GOOGLE_OAUTH_SCOPES = 'https://www.googleapis.com/auth/cloud-platform https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile';
  
  // Claude OAuth 2.0 configuration (from working opencode implementation)
  CLAUDE_OAUTH_CLIENT_ID = '9d1c250a-e61b-44d9-88ed-5944d1962f5e';
  CLAUDE_OAUTH_CLIENT_SECRET = ''; // Public client - no secret needed for PKCE
  CLAUDE_OAUTH_AUTH_ENDPOINT = 'https://claude.ai/oauth/authorize';
  CLAUDE_OAUTH_TOKEN_ENDPOINT = 'https://console.anthropic.com/v1/oauth/token';
  CLAUDE_OAUTH_SCOPES = 'org:create_api_key user:profile user:inference';
  
  // Backward compatibility aliases (for existing Gemini code)
  OAUTH_CLIENT_ID = GOOGLE_OAUTH_CLIENT_ID;
  OAUTH_CLIENT_SECRET = GOOGLE_OAUTH_CLIENT_SECRET;
  OAUTH_AUTH_ENDPOINT = GOOGLE_OAUTH_AUTH_ENDPOINT;
  OAUTH_TOKEN_ENDPOINT = GOOGLE_OAUTH_TOKEN_ENDPOINT;
  OAUTH_TOKEN_INFO_ENDPOINT = GOOGLE_OAUTH_TOKEN_INFO_ENDPOINT;
  OAUTH_SCOPES = GOOGLE_OAUTH_SCOPES;

implementation

constructor TOAuthClient.Create;
begin
  inherited Create;
  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;
  FHttpClient.AddHeader('User-Agent', 'gitpal-OAuth-Client/1.0');
end;

destructor TOAuthClient.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TOAuthClient.GenerateRandomHex(ByteCount: Integer): AnsiString;
var
  I: Integer;
  RandomByte: Byte;
begin
  Result := AnsiString('');
  Randomize;
  
  for I := 0 to ByteCount - 1 do
  begin
    RandomByte := Random(256);
    Result := Result + AnsiString(IntToHex(RandomByte, 2));
  end;
end;

function TOAuthClient.GenerateState: AnsiString;
begin
  // Generate 32 random bytes as hex string (64 characters)
  Result := GenerateRandomHex(32);
end;

function TOAuthClient.BuildAuthorizationURL(const RedirectURI, State: AnsiString): AnsiString;
begin
  Result := AnsiString(OAUTH_AUTH_ENDPOINT) +
    '?response_type=code' +
    '&client_id=' + AnsiString(OAUTH_CLIENT_ID) +
    '&redirect_uri=' + RedirectURI +
    '&scope=' + AnsiString(OAUTH_SCOPES) +
    '&state=' + State +
    '&access_type=offline' +
    '&prompt=consent';
end;

function TOAuthClient.ExchangeCodeForTokens(const AuthCode, RedirectURI: AnsiString): TOAuthTokens;
var
  PostData: AnsiString;
  Response: AnsiString;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
begin
  // Initialize result
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := AnsiString('');
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  try
    // Prepare POST data for token exchange
    PostData := 'grant_type=authorization_code' +
      '&code=' + AuthCode +
      '&redirect_uri=' + RedirectURI +
      '&client_id=' + AnsiString(OAUTH_CLIENT_ID) +
      '&client_secret=' + AnsiString(OAUTH_CLIENT_SECRET);
    
    FHttpClient.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    
    // Make the token exchange request
    Response := AnsiString(FHttpClient.FormPost(OAUTH_TOKEN_ENDPOINT, PostData));
    
    // Parse JSON response
    JsonData := GetJSON(string(Response));
    try
      if JsonData is TJSONObject then
      begin
        JsonObject := TJSONObject(JsonData);
        
        // Extract token information
        if JsonObject.Find('access_token') <> nil then
          Result.AccessToken := AnsiString(JsonObject.Get('access_token', ''));
          
        if JsonObject.Find('refresh_token') <> nil then
          Result.RefreshToken := AnsiString(JsonObject.Get('refresh_token', ''));
          
        if JsonObject.Find('token_type') <> nil then
          Result.TokenType := AnsiString(JsonObject.Get('token_type', ''));
          
        if JsonObject.Find('expires_in') <> nil then
        begin
          Result.ExpiresIn := JsonObject.Get('expires_in', 0);
          Result.ExpiresAt := DateTimeToUnix(Now) + Result.ExpiresIn;
        end;
        
        if JsonObject.Find('scope') <> nil then
          Result.Scope := AnsiString(JsonObject.Get('scope', ''));
      end;
    finally
      JsonData.Free;
    end;
    
  except
    on E: Exception do
    begin
      // Token exchange failed
      Result.AccessToken := AnsiString('');
      raise Exception.Create('Token exchange failed: ' + E.Message);
    end;
  end;
end;

function TOAuthClient.RefreshTokens(const RefreshToken: AnsiString): TOAuthTokens;
var
  PostData: AnsiString;
  Response: AnsiString;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
begin
  // Initialize result
  Result.AccessToken := AnsiString('');
  Result.RefreshToken := RefreshToken; // Keep the same refresh token
  Result.TokenType := AnsiString('');
  Result.ExpiresIn := 0;
  Result.ExpiresAt := 0;
  Result.Scope := AnsiString('');
  
  try
    // Prepare POST data for token refresh
    PostData := 'grant_type=refresh_token' +
      '&refresh_token=' + RefreshToken +
      '&client_id=' + AnsiString(OAUTH_CLIENT_ID) +
      '&client_secret=' + AnsiString(OAUTH_CLIENT_SECRET);
    
    FHttpClient.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    
    // Make the token refresh request
    Response := AnsiString(FHttpClient.FormPost(OAUTH_TOKEN_ENDPOINT, PostData));
    
    // Parse JSON response
    JsonData := GetJSON(string(Response));
    try
      if JsonData is TJSONObject then
      begin
        JsonObject := TJSONObject(JsonData);
        
        // Extract token information
        if JsonObject.Find('access_token') <> nil then
          Result.AccessToken := AnsiString(JsonObject.Get('access_token', ''));
          
        if JsonObject.Find('token_type') <> nil then
          Result.TokenType := AnsiString(JsonObject.Get('token_type', ''));
          
        if JsonObject.Find('expires_in') <> nil then
        begin
          Result.ExpiresIn := JsonObject.Get('expires_in', 0);
          Result.ExpiresAt := DateTimeToUnix(Now) + Result.ExpiresIn;
        end;
        
        if JsonObject.Find('scope') <> nil then
          Result.Scope := AnsiString(JsonObject.Get('scope', ''));
      end;
    finally
      JsonData.Free;
    end;
    
  except
    on E: Exception do
    begin
      // Token refresh failed
      Result.AccessToken := AnsiString('');
      raise Exception.Create('Token refresh failed: ' + E.Message);
    end;
  end;
end;

function TOAuthClient.ValidateToken(const AccessToken: AnsiString): Boolean;
var
  URL: AnsiString;
  Response: AnsiString;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
  RetryCount: Integer;
  MaxRetries: Integer;
begin
  Result := False;
  MaxRetries := 2;
  
  for RetryCount := 1 to MaxRetries do
  begin
    try
      // Make token info request
      URL := AnsiString(OAUTH_TOKEN_INFO_ENDPOINT) + '?access_token=' + AccessToken;
      Response := AnsiString(FHttpClient.Get(string(URL)));
      
      // Check HTTP status code
      if FHttpClient.ResponseStatusCode <> 200 then
      begin
        // Token is invalid or request failed
        if FHttpClient.ResponseStatusCode = 400 then
          Result := False // Invalid token
        else if (RetryCount < MaxRetries) and (FHttpClient.ResponseStatusCode >= 500) then
          Continue // Server error, retry
        else
          Result := False; // Other error or max retries reached
        Break;
      end;
      
      // Parse JSON response
      JsonData := GetJSON(string(Response));
      try
        if JsonData is TJSONObject then
        begin
          JsonObject := TJSONObject(JsonData);
          
          // Check if token is valid (should have audience field and no error)
          Result := (JsonObject.Find('aud') <> nil) and 
                    (JsonObject.Find('error') = nil);
        end;
      finally
        JsonData.Free;
      end;
      
      Break; // Success or valid response received
      
    except
      on E: Exception do
      begin
        // Network or other error
        if RetryCount < MaxRetries then
        begin
          Sleep(1000); // Wait 1 second before retry
          Continue;
        end
        else
        begin
          // Max retries reached, assume invalid token
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

end.
