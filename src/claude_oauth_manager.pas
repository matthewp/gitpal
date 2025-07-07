{$mode objfpc}
{$codepage UTF8}
{$H+}

unit claude_oauth_manager;

interface

uses
  SysUtils, Classes, DateUtils, models,
  claude_oauth_client, browser_launch,
  oauth_client, logging, config_manager,
  claude_oauth_provider;

type
  TClaudeOAuthManager = class(TInterfacedObject, ILLMProvider)
  private
    FOAuthClient: TClaudeOAuthClient;
    FConfig: TGitPalConfig;
    FCurrentTokens: TOAuthTokens;
    FTokensLoaded: Boolean;
    FClaudeProvider: claude_oauth_provider.TClaudeOAuthProvider;
    
    function LoadStoredTokens: Boolean;
    function RefreshTokensIfNeeded: Boolean;
    function PerformOAuthFlow: Boolean;
    procedure EnsureClaudeProvider;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // OAuth-specific methods
    function Authenticate: Boolean;
    function AuthenticateForce: Boolean; // Force new OAuth flow, ignore existing tokens
    function IsAuthenticated: Boolean;
    function GetCurrentAccessToken: AnsiString;
    function GetCurrentTokens: TOAuthTokens;
    function Logout: Boolean;
    
    // Implement ILLMProvider interface by delegating to Claude provider
    function ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    procedure ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
      ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
    function GetProviderName: string;
    function GetDefaultModel: string;
    function ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    function SupportsToolCalling: Boolean;
  end;

const
  CLAUDE_OAUTH_PROVIDER_NAME = 'claude';

implementation

constructor TClaudeOAuthManager.Create;
begin
  inherited Create;
  DebugLog('TClaudeOAuthProvider.Create: Starting constructor');
  
  FOAuthClient := TClaudeOAuthClient.Create;
  DebugLog('TClaudeOAuthProvider.Create: OAuth client created');
  
  FConfig := TGitPalConfig.Create;
  FConfig.LoadConfig;
  DebugLog('TClaudeOAuthProvider.Create: Config loaded');
  
  FTokensLoaded := False;
  FClaudeProvider := nil; // Will be created when tokens are available
  
  // Initialize empty tokens
  FillChar(FCurrentTokens, SizeOf(FCurrentTokens), 0);
  FCurrentTokens.AccessToken := AnsiString('');
  FCurrentTokens.RefreshToken := AnsiString('');
  FCurrentTokens.TokenType := AnsiString('');
  FCurrentTokens.ExpiresIn := 0;
  FCurrentTokens.ExpiresAt := 0;
  FCurrentTokens.Scope := AnsiString('');
  
  DebugLog('TClaudeOAuthProvider.Create: Constructor completed');
end;

destructor TClaudeOAuthManager.Destroy;
begin
  if Assigned(FClaudeProvider) then
    FClaudeProvider.Free;
  if Assigned(FConfig) then
    FConfig.Free;
  if Assigned(FOAuthClient) then
    FOAuthClient.Free;
  inherited Destroy;
end;

function TClaudeOAuthManager.LoadStoredTokens: Boolean;
var
  ClaudeConfig: TProviderConfig;
begin
  Result := False;
  
  if FTokensLoaded then
  begin
    Result := True;
    Exit;
  end;
  
  try
    ClaudeConfig := FConfig.GetProviderConfig(PROVIDER_CLAUDE);
    
    // Check if we have OAuth tokens in config
    if (ClaudeConfig.AuthMethod = amOAuth) and 
       (ClaudeConfig.OAuthAccessToken <> '') and 
       (ClaudeConfig.OAuthRefreshToken <> '') then
    begin
      // Load tokens from config
      FCurrentTokens.AccessToken := ClaudeConfig.OAuthAccessToken;
      FCurrentTokens.RefreshToken := ClaudeConfig.OAuthRefreshToken;
      FCurrentTokens.TokenType := ClaudeConfig.OAuthTokenType;
      FCurrentTokens.ExpiresAt := ClaudeConfig.OAuthExpiresAt;
      FCurrentTokens.Scope := ClaudeConfig.OAuthScope;
      FCurrentTokens.ExpiresIn := 0; // Not needed for refresh logic
      
      FTokensLoaded := True;
      Result := True;
      DebugLog('LoadStoredTokens: Loaded OAuth tokens from config');
    end
    else
    begin
      DebugLog('LoadStoredTokens: No valid OAuth tokens found in config');
    end;
  except
    on E: Exception do
    begin
      DebugLog('LoadStoredTokens: Exception loading tokens: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TClaudeOAuthManager.RefreshTokensIfNeeded: Boolean;
var
  CurrentTime: Int64;
  BufferTime: Int64;
  NewTokens: TOAuthTokens;
  ClaudeConfig: TProviderConfig;
begin
  DebugLog('RefreshTokensIfNeeded: Starting');
  Result := True;
  
  if not FTokensLoaded then
  begin
    DebugLog('RefreshTokensIfNeeded: Tokens not loaded, returning False');
    Exit(False);
  end;
  
  // Check if token expires within 5 minutes
  CurrentTime := DateTimeToUnix(Now);
  BufferTime := 5 * 60; // 5 minutes in seconds
  
  DebugLog('RefreshTokensIfNeeded: CurrentTime = ' + IntToStr(CurrentTime));
  DebugLog('RefreshTokensIfNeeded: Token ExpiresAt = ' + IntToStr(FCurrentTokens.ExpiresAt));
  DebugLog('RefreshTokensIfNeeded: Buffer time = ' + IntToStr(BufferTime));
  
  if (FCurrentTokens.ExpiresAt > 0) and (FCurrentTokens.ExpiresAt > (CurrentTime + BufferTime)) then
  begin
    // Token is still valid
    DebugLog('RefreshTokensIfNeeded: Token is still valid, no refresh needed');
    Exit(True);
  end;
  
  // Need to refresh
  try
    NewTokens := FOAuthClient.RefreshTokens(FCurrentTokens.RefreshToken);
    
    if NewTokens.AccessToken <> '' then
    begin
      FCurrentTokens := NewTokens;
      
      // Save the refreshed tokens to config
      ClaudeConfig := FConfig.GetProviderConfig(PROVIDER_CLAUDE);
      ClaudeConfig.OAuthAccessToken := NewTokens.AccessToken;
      ClaudeConfig.OAuthRefreshToken := NewTokens.RefreshToken;
      ClaudeConfig.OAuthTokenType := NewTokens.TokenType;
      ClaudeConfig.OAuthExpiresAt := NewTokens.ExpiresAt;
      ClaudeConfig.OAuthScope := NewTokens.Scope;
      FConfig.SetProviderConfig(PROVIDER_CLAUDE, ClaudeConfig);
      
      try
        FConfig.SaveConfig;
      except
        on E: Exception do
        begin
          DebugLog('Warning: Failed to save refreshed OAuth tokens: ' + E.Message);
        end;
      end;
      
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  except
    on E: Exception do
    begin
      DebugLog('Token refresh failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TClaudeOAuthManager.PerformOAuthFlow: Boolean;
var
  State: AnsiString;
  AuthURL: AnsiString;
  Tokens: TOAuthTokens;
  LaunchResult: TBrowserLaunchResult;
  AuthCode: string;
begin
  Result := False;
  
  // Generate state for CSRF protection
  State := FOAuthClient.GenerateState;
  
  // Build authorization URL with Anthropic's official redirect URI
  AuthURL := FOAuthClient.BuildAuthorizationURL(AnsiString('https://console.anthropic.com/oauth/code/callback'), State);
  
  // Open browser
  WriteLn('Opening browser for Claude authentication...');
  WriteLn('If browser doesn''t open, visit: ', string(AuthURL));
  WriteLn('');
  
  LaunchResult := LaunchBrowser(AuthURL);
  if not LaunchResult.Success then
  begin
    WriteLn('Failed to open browser automatically: ', string(LaunchResult.ErrorMessage));
    WriteLn('Please open the following URL manually:');
    WriteLn(string(AuthURL));
    WriteLn('');
  end;
  
  // Ask user to copy the authorization code from the callback URL
  WriteLn('After clicking "Authorize", you''ll be redirected to a page.');
  WriteLn('Copy the authorization code from the URL (the "code" parameter) and paste it here.');
  WriteLn('');
  Write('Authorization code (including the # part): ');
  ReadLn(AuthCode);
  
  AuthCode := Trim(AuthCode);
  
  if AuthCode = '' then
  begin
    WriteLn('No authorization code provided');
    Exit;
  end;
  
  WriteLn('Exchanging authorization code for tokens...');
  
  // Exchange code for tokens
  try
    Tokens := FOAuthClient.ExchangeCodeForTokens(AnsiString(AuthCode), AnsiString('https://console.anthropic.com/oauth/code/callback'));
      
      if Tokens.AccessToken = '' then
      begin
        WriteLn('Error: Failed to obtain access token');
        Exit;
      end;
      
      // Save tokens
      FCurrentTokens := Tokens;
      FTokensLoaded := True;
      
      // Just store tokens in memory, don't save to config
      // The setup wizard will save everything at once
      WriteLn('Claude OAuth authentication successful!');
      Result := True;
      
    except
      on E: Exception do
      begin
        WriteLn('Token exchange failed: ', E.Message);
      end;
    end;
end;

function TClaudeOAuthManager.Authenticate: Boolean;
begin
  DebugLog('TClaudeOAuthProvider.Authenticate: Starting authentication');
  
  // Try to load existing tokens first
  DebugLog('TClaudeOAuthProvider.Authenticate: Checking for stored tokens');
  if LoadStoredTokens then
  begin
    DebugLog('TClaudeOAuthProvider.Authenticate: Found stored tokens, checking if refresh needed');
    // Try to refresh if needed
    if RefreshTokensIfNeeded then
    begin
      DebugLog('TClaudeOAuthProvider.Authenticate: Token refresh successful, authentication complete');
      Result := True;
      Exit;
    end
    else
    begin
      DebugLog('TClaudeOAuthProvider.Authenticate: Token refresh failed, proceeding with OAuth flow');
    end;
  end
  else
  begin
    DebugLog('TClaudeOAuthProvider.Authenticate: No stored tokens found, proceeding with OAuth flow');
  end;
  
  // Need to perform OAuth flow
  DebugLog('TClaudeOAuthProvider.Authenticate: Starting OAuth flow');
  Result := PerformOAuthFlow;
end;

function TClaudeOAuthManager.AuthenticateForce: Boolean;
begin
  DebugLog('TClaudeOAuthProvider.AuthenticateForce: Starting forced authentication (ignoring existing tokens)');
  
  // Clear any existing tokens to force new OAuth flow
  FillChar(FCurrentTokens, SizeOf(FCurrentTokens), 0);
  FCurrentTokens.AccessToken := AnsiString('');
  FCurrentTokens.RefreshToken := AnsiString('');
  FCurrentTokens.TokenType := AnsiString('');
  FCurrentTokens.ExpiresIn := 0;
  FCurrentTokens.ExpiresAt := 0;
  FCurrentTokens.Scope := AnsiString('');
  FTokensLoaded := False;
  
  // Force OAuth flow
  DebugLog('TClaudeOAuthProvider.AuthenticateForce: Starting OAuth flow');
  Result := PerformOAuthFlow;
end;

function TClaudeOAuthManager.IsAuthenticated: Boolean;
begin
  DebugLog('TClaudeOAuthProvider.IsAuthenticated: Checking authentication status');
  Result := LoadStoredTokens and RefreshTokensIfNeeded;
  DebugLog('TClaudeOAuthProvider.IsAuthenticated: Result = ' + BoolToStr(Result, True));
end;

function TClaudeOAuthManager.GetCurrentAccessToken: AnsiString;
begin
  DebugLog('TClaudeOAuthProvider.GetCurrentAccessToken: Starting');
  if IsAuthenticated then
  begin
    Result := FCurrentTokens.AccessToken;
    DebugLog('TClaudeOAuthProvider.GetCurrentAccessToken: Returning token, length = ' + IntToStr(Length(Result)));
  end
  else
  begin
    Result := AnsiString('');
    DebugLog('TClaudeOAuthProvider.GetCurrentAccessToken: Not authenticated, returning empty token');
  end;
end;

function TClaudeOAuthManager.GetCurrentTokens: TOAuthTokens;
begin
  Result := FCurrentTokens;
end;

function TClaudeOAuthManager.Logout: Boolean;
var
  ClaudeConfig: TProviderConfig;
begin
  try
    // Clear tokens from config
    ClaudeConfig := FConfig.GetProviderConfig(PROVIDER_CLAUDE);
    ClaudeConfig.AuthMethod := amApiKey; // Reset to API key method
    ClaudeConfig.OAuthAccessToken := AnsiString('');
    ClaudeConfig.OAuthRefreshToken := AnsiString('');
    ClaudeConfig.OAuthTokenType := AnsiString('');
    ClaudeConfig.OAuthExpiresAt := 0;
    ClaudeConfig.OAuthScope := AnsiString('');
    FConfig.SetProviderConfig(PROVIDER_CLAUDE, ClaudeConfig);
    
    try
      FConfig.SaveConfig;
      Result := True;
    except
      Result := False;
    end;
    
    // Clear in-memory tokens
    FillChar(FCurrentTokens, SizeOf(FCurrentTokens), 0);
    FCurrentTokens.AccessToken := AnsiString('');
    FCurrentTokens.RefreshToken := AnsiString('');
    FCurrentTokens.TokenType := AnsiString('');
    FCurrentTokens.ExpiresIn := 0;
    FCurrentTokens.ExpiresAt := 0;
    FCurrentTokens.Scope := AnsiString('');
    FTokensLoaded := False;
    
    // Clear Claude provider
    if Assigned(FClaudeProvider) then
    begin
      FClaudeProvider.Free;
      FClaudeProvider := nil;
    end;
    
    if Result then
      WriteLn('Successfully logged out of Claude OAuth');
  except
    on E: Exception do
    begin
      WriteLn('Logout failed: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TClaudeOAuthManager.EnsureClaudeProvider;
var
  AccessToken: AnsiString;
begin
  DebugLog('EnsureClaudeProvider: Starting');
  
  if not IsAuthenticated then
  begin
    DebugLog('EnsureClaudeProvider: Not authenticated, raising exception');
    raise Exception.Create('Not authenticated. Please run authentication first.');
  end;
  
  AccessToken := GetCurrentAccessToken;
  DebugLog('EnsureClaudeProvider: AccessToken length = ' + IntToStr(Length(AccessToken)));
  
  if AccessToken = '' then
  begin
    DebugLog('EnsureClaudeProvider: Access token is empty, raising exception');
    raise Exception.Create('Failed to get valid access token');
  end;
  
  // Create or recreate Claude provider with current access token
  if Assigned(FClaudeProvider) then
  begin
    DebugLog('EnsureClaudeProvider: Freeing existing Claude provider');
    FClaudeProvider.Free;
  end;
  
  DebugLog('EnsureClaudeProvider: Creating new TClaudeOAuthProvider with access token');
  FClaudeProvider := claude_oauth_provider.TClaudeOAuthProvider.Create(string(AccessToken), 'You are Claude Code, Anthropic''s official CLI for Claude.');
  DebugLog('EnsureClaudeProvider: Claude provider created successfully');
end;

// Implement ILLMProvider interface by delegating to Claude provider

function TClaudeOAuthManager.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
  ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
var
  ModifiedMessages: array of TLLMMessage;
  SystemMessageContent: string;
  I, J: Integer;
begin
  DebugLog('TClaudeOAuthManager.ChatCompletion: Starting');
  DebugLog('TClaudeOAuthManager.ChatCompletion: Model = ' + AModel);
  DebugLog('TClaudeOAuthManager.ChatCompletion: Messages count = ' + IntToStr(Length(AMessages)));
  
  // Transform messages like the old adapter did:
  // 1. Extract system message content and convert to first user message
  // 2. Skip system messages, add other messages after
  
  SystemMessageContent := '';
  
  // Find system message content
  for I := Low(AMessages) to High(AMessages) do
  begin
    if AMessages[I].Role = lmrSystem then
    begin
      SystemMessageContent := AMessages[I].Content;
      Break;
    end;
  end;
  
  // Build new message array with instructions as first user message
  J := 0;
  if SystemMessageContent <> '' then
  begin
    SetLength(ModifiedMessages, Length(AMessages)); // Start with same size, may shrink
    
    // Add commit instructions as first user message
    ModifiedMessages[J].Role := lmrUser;
    ModifiedMessages[J].Content := SystemMessageContent;
    ModifiedMessages[J].Name := '';
    ModifiedMessages[J].ToolCallId := '';
    ModifiedMessages[J].FunctionName := '';
    SetLength(ModifiedMessages[J].ToolCalls, 0);
    Inc(J);
    
    // Add all non-system messages
    for I := Low(AMessages) to High(AMessages) do
    begin
      if AMessages[I].Role <> lmrSystem then
      begin
        ModifiedMessages[J] := AMessages[I];
        Inc(J);
      end;
    end;
    
    // Resize array to actual size
    SetLength(ModifiedMessages, J);
  end
  else
  begin
    // No system message, just copy all messages
    SetLength(ModifiedMessages, Length(AMessages));
    for I := Low(AMessages) to High(AMessages) do
      ModifiedMessages[I] := AMessages[I];
  end;
  
  DebugLog('TClaudeOAuthManager.ChatCompletion: Modified messages count = ' + IntToStr(Length(ModifiedMessages)));
  
  EnsureClaudeProvider;
  Result := FClaudeProvider.ChatCompletion(AModel, ModifiedMessages, ATemperature, AMaxTokens);
  
  if Length(Result.Choices) > 0 then
  begin
    DebugLog('TClaudeOAuthManager.ChatCompletion: Response length = ' + IntToStr(Length(Result.Choices[0].Message.Content)));
    DebugLog('TClaudeOAuthManager.ChatCompletion: Response preview = ' + Copy(Result.Choices[0].Message.Content, 1, 200));
  end;
end;

procedure TClaudeOAuthManager.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
  ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
var
  ModifiedMessages: array of TLLMMessage;
  SystemMessageContent: string;
  I, J: Integer;
begin
  DebugLog('TClaudeOAuthManager.ChatCompletionStream: Starting');
  DebugLog('TClaudeOAuthManager.ChatCompletionStream: Model = ' + AModel);
  DebugLog('TClaudeOAuthManager.ChatCompletionStream: Messages count = ' + IntToStr(Length(AMessages)));
  
  // Transform messages like the old adapter did:
  // 1. Extract system message content and convert to first user message
  // 2. Skip system messages, add other messages after
  
  SystemMessageContent := '';
  
  // Find system message content
  for I := Low(AMessages) to High(AMessages) do
  begin
    if AMessages[I].Role = lmrSystem then
    begin
      SystemMessageContent := AMessages[I].Content;
      Break;
    end;
  end;
  
  // Build new message array with instructions as first user message
  J := 0;
  if SystemMessageContent <> '' then
  begin
    SetLength(ModifiedMessages, Length(AMessages)); // Start with same size, may shrink
    
    // Add commit instructions as first user message
    ModifiedMessages[J].Role := lmrUser;
    ModifiedMessages[J].Content := SystemMessageContent;
    ModifiedMessages[J].Name := '';
    ModifiedMessages[J].ToolCallId := '';
    ModifiedMessages[J].FunctionName := '';
    SetLength(ModifiedMessages[J].ToolCalls, 0);
    Inc(J);
    
    // Add all non-system messages
    for I := Low(AMessages) to High(AMessages) do
    begin
      if AMessages[I].Role <> lmrSystem then
      begin
        ModifiedMessages[J] := AMessages[I];
        Inc(J);
      end;
    end;
    
    // Resize array to actual size
    SetLength(ModifiedMessages, J);
  end
  else
  begin
    // No system message, just copy all messages
    SetLength(ModifiedMessages, Length(AMessages));
    for I := Low(AMessages) to High(AMessages) do
      ModifiedMessages[I] := AMessages[I];
  end;
  
  DebugLog('TClaudeOAuthManager.ChatCompletionStream: Modified messages count = ' + IntToStr(Length(ModifiedMessages)));
  
  try
    EnsureClaudeProvider;
    DebugLog('TClaudeOAuthManager.ChatCompletionStream: Delegating to FClaudeProvider');
    FClaudeProvider.ChatCompletionStream(AModel, ModifiedMessages, ACallback, ATemperature, AMaxTokens);
    DebugLog('TClaudeOAuthManager.ChatCompletionStream: Delegation completed');
  except
    on E: Exception do
    begin
      DebugLog('TClaudeOAuthManager.ChatCompletionStream: Exception: ' + E.Message);
      raise;
    end;
  end;
end;

function TClaudeOAuthManager.GetProviderName: string;
begin
  Result := 'claude-oauth';
end;

function TClaudeOAuthManager.GetDefaultModel: string;
begin
  Result := 'claude-3-5-sonnet-latest';
end;

function TClaudeOAuthManager.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
var
  ModifiedMessages: array of TLLMMessage;
  SystemMessageContent: string;
  I, J: Integer;
begin
  DebugLog('TClaudeOAuthManager.ChatCompletionWithTools: Starting');
  DebugLog('TClaudeOAuthManager.ChatCompletionWithTools: Model = ' + AModel);
  DebugLog('TClaudeOAuthManager.ChatCompletionWithTools: Messages count = ' + IntToStr(Length(AMessages)));
  
  // Transform messages like the old adapter did:
  // 1. Extract system message content and convert to first user message
  // 2. Skip system messages, add other messages after
  
  SystemMessageContent := '';
  
  // Find system message content
  for I := Low(AMessages) to High(AMessages) do
  begin
    if AMessages[I].Role = lmrSystem then
    begin
      SystemMessageContent := AMessages[I].Content;
      Break;
    end;
  end;
  
  // Build new message array with instructions as first user message
  J := 0;
  if SystemMessageContent <> '' then
  begin
    SetLength(ModifiedMessages, Length(AMessages)); // Start with same size, may shrink
    
    // Add commit instructions as first user message
    ModifiedMessages[J].Role := lmrUser;
    ModifiedMessages[J].Content := SystemMessageContent;
    ModifiedMessages[J].Name := '';
    ModifiedMessages[J].ToolCallId := '';
    ModifiedMessages[J].FunctionName := '';
    SetLength(ModifiedMessages[J].ToolCalls, 0);
    Inc(J);
    
    // Add all non-system messages
    for I := Low(AMessages) to High(AMessages) do
    begin
      if AMessages[I].Role <> lmrSystem then
      begin
        ModifiedMessages[J] := AMessages[I];
        Inc(J);
      end;
    end;
    
    // Resize array to actual size
    SetLength(ModifiedMessages, J);
  end
  else
  begin
    // No system message, just copy all messages
    SetLength(ModifiedMessages, Length(AMessages));
    for I := Low(AMessages) to High(AMessages) do
      ModifiedMessages[I] := AMessages[I];
  end;
  
  DebugLog('TClaudeOAuthManager.ChatCompletionWithTools: Modified messages count = ' + IntToStr(Length(ModifiedMessages)));
  
  EnsureClaudeProvider;
  Result := FClaudeProvider.ChatCompletionWithTools(AModel, ModifiedMessages, ATools, AToolContext, ATemperature, AMaxTokens);
end;

function TClaudeOAuthManager.SupportsToolCalling: Boolean;
begin
  Result := True; // Claude supports tool calling
end;

end.