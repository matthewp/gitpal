{$mode objfpc}
{$codepage UTF8}
{$H+}

unit gitpal_auth;

interface

uses
  SysUtils, DateUtils, token_storage, oauth_client, gemini_provider, models;

type
  TGitPalAuthProvider = class
  private
    FTokenStorage: TTokenStorage;
    FOAuthClient: TOAuthClient;
    function GetValidAccessToken: AnsiString;
    function RefreshTokenIfNeeded(const Tokens: TOAuthTokens): TOAuthTokens;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAuthenticatedProvider: TGeminiProvider;
    function HasValidAuthentication: Boolean;
    function GetAuthenticationMethod: AnsiString; // 'oauth' or 'api_key' or 'none'
    function IsUsingOAuth: Boolean;
  end;

implementation


constructor TGitPalAuthProvider.Create;
begin
  inherited Create;
  FTokenStorage := TTokenStorage.Create;
  FOAuthClient := TOAuthClient.Create;
end;

destructor TGitPalAuthProvider.Destroy;
begin
  FTokenStorage.Free;
  FOAuthClient.Free;
  inherited Destroy;
end;

function TGitPalAuthProvider.GetValidAccessToken: AnsiString;
var
  Tokens: TOAuthTokens;
  RefreshedTokens: TOAuthTokens;
  CurrentTime: Int64;
begin
  Result := AnsiString('');
  
  // Load existing tokens
  Tokens := FTokenStorage.LoadTokens;
  
  if Tokens.AccessToken = '' then
    Exit; // No tokens available
  
  // Check if token is expired or about to expire (5 minute buffer)
  CurrentTime := DateTimeToUnix(Now);
  if (Tokens.ExpiresAt > 0) and (Tokens.ExpiresAt <= CurrentTime + 300) then
  begin
    // Try to refresh the token
    if Tokens.RefreshToken <> '' then
    begin
      try
        RefreshedTokens := FOAuthClient.RefreshTokens(Tokens.RefreshToken);
        if RefreshedTokens.AccessToken <> '' then
        begin
          // Update the refresh token if a new one was provided
          if RefreshedTokens.RefreshToken = '' then
            RefreshedTokens.RefreshToken := Tokens.RefreshToken;
            
          // Save the refreshed tokens
          if FTokenStorage.SaveTokens(RefreshedTokens) then
          begin
            Result := RefreshedTokens.AccessToken;
            Exit;
          end;
        end;
      except
        // Refresh failed, fall through to return empty result
      end;
    end;
    
    // Token expired and couldn't refresh
    Exit;
  end;
  
  // Token is still valid
  Result := Tokens.AccessToken;
end;

function TGitPalAuthProvider.RefreshTokenIfNeeded(const Tokens: TOAuthTokens): TOAuthTokens;
var
  CurrentTime: Int64;
begin
  Result := Tokens;
  
  // Check if token needs refresh (5 minute buffer)
  CurrentTime := DateTimeToUnix(Now);
  if (Tokens.ExpiresAt > 0) and (Tokens.ExpiresAt <= CurrentTime + 300) then
  begin
    if Tokens.RefreshToken <> '' then
    begin
      try
        Result := FOAuthClient.RefreshTokens(Tokens.RefreshToken);
        if Result.AccessToken <> '' then
        begin
          // Keep original refresh token if new one not provided
          if Result.RefreshToken = '' then
            Result.RefreshToken := Tokens.RefreshToken;
            
          // Save refreshed tokens
          FTokenStorage.SaveTokens(Result);
        end
        else
        begin
          // Refresh failed, return original tokens
          Result := Tokens;
        end;
      except
        // Refresh failed, return original tokens
        Result := Tokens;
      end;
    end;
  end;
end;


function TGitPalAuthProvider.GetAuthenticatedProvider: TGeminiProvider;
var
  ApiKey: AnsiString;
begin
  Result := nil;
  
  // For now, only support API key authentication until OAuth is properly configured
  // OAuth infrastructure remains for future use
  try
    ApiKey := AnsiString(GetEnvironmentVariable('GEMINI_API_KEY'));
    if ApiKey <> '' then
    begin
      // Create provider with API key authentication (standard Gemini API)
      Result := TGeminiProvider.Create(string(ApiKey));
      Exit;
    end;
  except
    on E: Exception do
    begin
      // API key provider creation failed
      if Assigned(Result) then
      begin
        Result.Free;
        Result := nil;
      end;
      raise;
    end;
  end;
  
  // No authentication available
  raise Exception.Create(
    'Authentication required. Please set the GEMINI_API_KEY environment variable.'
  );
end;

function TGitPalAuthProvider.HasValidAuthentication: Boolean;
var
  ApiKey: AnsiString;
begin
  // For now, only check API key until OAuth is properly configured
  ApiKey := AnsiString(GetEnvironmentVariable('GEMINI_API_KEY'));
  Result := ApiKey <> '';
end;

function TGitPalAuthProvider.GetAuthenticationMethod: AnsiString;
var
  ApiKey: AnsiString;
begin
  // For now, only support API key until OAuth is properly configured
  ApiKey := AnsiString(GetEnvironmentVariable('GEMINI_API_KEY'));
  if ApiKey <> '' then
    Result := AnsiString('api_key')
  else
    Result := AnsiString('none');
end;

function TGitPalAuthProvider.IsUsingOAuth: Boolean;
begin
  Result := GetAuthenticationMethod = 'oauth';
end;

end.
