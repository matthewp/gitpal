{$mode objfpc}
{$codepage UTF8}
{$H+}

unit config_manager;

interface

uses
  SysUtils, Classes, fpjson, jsonparser, BaseUnix;

type
  TAuthMethod = (amApiKey, amOAuth);
  
  TProviderConfig = record
    Enabled: Boolean;
    ApiKey: AnsiString;
    Model: AnsiString;
    AuthMethod: TAuthMethod;
    // OAuth tokens (only used when AuthMethod = amOAuth)
    OAuthAccessToken: AnsiString;
    OAuthRefreshToken: AnsiString;
    OAuthTokenType: AnsiString;
    OAuthExpiresAt: Int64; // Unix timestamp
    OAuthScope: AnsiString;
  end;
  
  
  TGitPalConfig = class
  private
    FConfigPath: AnsiString;
    FVersion: AnsiString;
    FDefaultProvider: AnsiString;
    FOpenAIConfig: TProviderConfig;
    FClaudeConfig: TProviderConfig;
    FGeminiConfig: TProviderConfig;
    
    function GetConfigDir: AnsiString;
    function EnsureConfigDir: Boolean;
    procedure InitializeDefaults;
    function LoadFromJSON(const JsonObj: TJSONObject): Boolean;
    function SaveToJSON: TJSONObject;
    procedure SetSecureFilePermissions(const FilePath: AnsiString);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Configuration management
    function LoadConfig: Boolean;
    procedure SaveConfig;
    function HasValidConfig: Boolean;
    function ConfigExists: Boolean;
    procedure ClearConfig;
    
    // Provider management
    function GetProviderConfig(const ProviderName: AnsiString): TProviderConfig;
    procedure SetProviderConfig(const ProviderName: AnsiString; const Config: TProviderConfig);
    function HasProviderApiKey(const ProviderName: AnsiString): Boolean;
    function GetProviderApiKey(const ProviderName: AnsiString): AnsiString;
    
    // Environment variable detection (for setup wizard)
    function DetectEnvironmentApiKey(const ProviderName: AnsiString): AnsiString;
    
    // Properties
    property Version: AnsiString read FVersion write FVersion;
    property DefaultProvider: AnsiString read FDefaultProvider write FDefaultProvider;
    property ConfigPath: AnsiString read FConfigPath;
  end;

const
  CONFIG_VERSION = '1.0';
  CONFIG_FILE_NAME = 'config.json';
  
  // Provider names
  PROVIDER_OPENAI = 'openai';
  PROVIDER_CLAUDE = 'claude';
  PROVIDER_GEMINI = 'gemini';
  
  // Environment variable names
  ENV_OPENAI_API_KEY = 'OPENAI_API_KEY';
  ENV_ANTHROPIC_API_KEY = 'ANTHROPIC_API_KEY';
  ENV_GEMINI_API_KEY = 'GEMINI_API_KEY';

implementation

constructor TGitPalConfig.Create;
begin
  inherited Create;
  FConfigPath := GetConfigDir + CONFIG_FILE_NAME;
  InitializeDefaults;
end;

destructor TGitPalConfig.Destroy;
begin
  inherited Destroy;
end;

function TGitPalConfig.GetConfigDir: AnsiString;
var
  XDGConfigHome: AnsiString;
  HomeDir: AnsiString;
begin
  // Follow XDG Base Directory Specification
  XDGConfigHome := AnsiString(GetEnvironmentVariable('XDG_CONFIG_HOME'));
  
  if XDGConfigHome <> '' then
    Result := XDGConfigHome + '/gitpal/'
  else
  begin
    HomeDir := AnsiString(GetEnvironmentVariable('HOME'));
    if HomeDir <> '' then
      Result := HomeDir + '/.config/gitpal/'
    else
      Result := './gitpal/'; // Fallback to current directory
  end;
end;

function TGitPalConfig.EnsureConfigDir: Boolean;
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
      
      // Set secure permissions on the config directory (700 - owner only)
      {$IFDEF UNIX}
      if FpChmod(PChar(ConfigDir), &700) <> 0 then
      begin
        // Non-fatal, continue anyway
      end;
      {$ENDIF}
    end;
    
    Result := True;
  except
    Result := False;
  end;
end;

procedure TGitPalConfig.InitializeDefaults;
begin
  FVersion := AnsiString(CONFIG_VERSION);
  FDefaultProvider := AnsiString('');
  
  // Initialize provider configs
  FOpenAIConfig.Enabled := True;
  FOpenAIConfig.ApiKey := AnsiString('');
  FOpenAIConfig.Model := AnsiString('gpt-4o');
  FOpenAIConfig.AuthMethod := amApiKey;
  
  FClaudeConfig.Enabled := True;
  FClaudeConfig.ApiKey := AnsiString('');
  FClaudeConfig.Model := AnsiString('claude-3-5-sonnet-latest');
  FClaudeConfig.AuthMethod := amApiKey;
  
  FGeminiConfig.Enabled := True;
  FGeminiConfig.ApiKey := AnsiString('');
  FGeminiConfig.Model := AnsiString('gemini-2.0-flash-exp');
  FGeminiConfig.AuthMethod := amApiKey;
  
end;

function TGitPalConfig.LoadFromJSON(const JsonObj: TJSONObject): Boolean;
var
  ProvidersObj, ProviderObj: TJSONObject;
begin
  Result := False;
  
  try
    // Load version
    if JsonObj.Find('version') <> nil then
      FVersion := JsonObj.Get('version', string(CONFIG_VERSION));
    
    // Load default provider
    if JsonObj.Find('defaultProvider') <> nil then
      FDefaultProvider := JsonObj.Get('defaultProvider', '');
    
    // Load providers
    if JsonObj.Find('providers') <> nil then
    begin
      ProvidersObj := JsonObj.Objects['providers'];
      
      // OpenAI
      if ProvidersObj.Find('openai') <> nil then
      begin
        ProviderObj := ProvidersObj.Objects['openai'];
        FOpenAIConfig.Enabled := ProviderObj.Get('enabled', True);
        FOpenAIConfig.ApiKey := ProviderObj.Get('apiKey', '');
        FOpenAIConfig.Model := ProviderObj.Get('model', 'gpt-4o');
        if ProviderObj.Get('authMethod', 'api_key') = 'oauth' then
          FOpenAIConfig.AuthMethod := amOAuth
        else
          FOpenAIConfig.AuthMethod := amApiKey;
      end;
      
      // Claude
      if ProvidersObj.Find('claude') <> nil then
      begin
        ProviderObj := ProvidersObj.Objects['claude'];
        FClaudeConfig.Enabled := ProviderObj.Get('enabled', True);
        FClaudeConfig.ApiKey := ProviderObj.Get('apiKey', '');
        FClaudeConfig.Model := ProviderObj.Get('model', 'claude-3-5-sonnet-latest');
        if ProviderObj.Get('authMethod', 'api_key') = 'oauth' then
          FClaudeConfig.AuthMethod := amOAuth
        else
          FClaudeConfig.AuthMethod := amApiKey;
        
        // Load OAuth tokens if present
        FClaudeConfig.OAuthAccessToken := ProviderObj.Get('oauthAccessToken', '');
        FClaudeConfig.OAuthRefreshToken := ProviderObj.Get('oauthRefreshToken', '');
        FClaudeConfig.OAuthTokenType := ProviderObj.Get('oauthTokenType', '');
        FClaudeConfig.OAuthExpiresAt := ProviderObj.Get('oauthExpiresAt', Int64(0));
        FClaudeConfig.OAuthScope := ProviderObj.Get('oauthScope', '');
      end;
      
      // Gemini
      if ProvidersObj.Find('gemini') <> nil then
      begin
        ProviderObj := ProvidersObj.Objects['gemini'];
        FGeminiConfig.Enabled := ProviderObj.Get('enabled', True);
        FGeminiConfig.ApiKey := ProviderObj.Get('apiKey', '');
        FGeminiConfig.Model := ProviderObj.Get('model', 'gemini-2.0-flash-exp');
        if ProviderObj.Get('authMethod', 'api_key') = 'oauth' then
          FGeminiConfig.AuthMethod := amOAuth
        else
          FGeminiConfig.AuthMethod := amApiKey;
      end;
    end;
    
    
    Result := True;
  except
    // JSON parsing failed
    Result := False;
  end;
end;

function TGitPalConfig.SaveToJSON: TJSONObject;
var
  ProvidersObj, OpenAIObj, ClaudeObj, GeminiObj: TJSONObject;
begin
  Result := TJSONObject.Create;
  
  // Basic fields
  Result.Add('version', string(FVersion));
  Result.Add('defaultProvider', string(FDefaultProvider));
  
  // Providers - only save enabled providers
  ProvidersObj := TJSONObject.Create;
  
  // OpenAI
  if FOpenAIConfig.Enabled and ((FOpenAIConfig.ApiKey <> '') or (FOpenAIConfig.AuthMethod = amOAuth)) then
  begin
    OpenAIObj := TJSONObject.Create;
    OpenAIObj.Add('enabled', FOpenAIConfig.Enabled);
    
    // Only add API key if not using OAuth
    if FOpenAIConfig.AuthMethod <> amOAuth then
      OpenAIObj.Add('apiKey', string(FOpenAIConfig.ApiKey));
    
    OpenAIObj.Add('model', string(FOpenAIConfig.Model));
    if FOpenAIConfig.AuthMethod = amOAuth then
      OpenAIObj.Add('authMethod', 'oauth')
    else
      OpenAIObj.Add('authMethod', 'api_key');
    ProvidersObj.Add('openai', OpenAIObj);
  end;
  
  // Claude
  if FClaudeConfig.Enabled and ((FClaudeConfig.ApiKey <> '') or (FClaudeConfig.AuthMethod = amOAuth)) then
  begin
    ClaudeObj := TJSONObject.Create;
    ClaudeObj.Add('enabled', FClaudeConfig.Enabled);
    
    // Only add API key if not using OAuth
    if FClaudeConfig.AuthMethod <> amOAuth then
      ClaudeObj.Add('apiKey', string(FClaudeConfig.ApiKey));
    
    ClaudeObj.Add('model', string(FClaudeConfig.Model));
    if FClaudeConfig.AuthMethod = amOAuth then
      ClaudeObj.Add('authMethod', 'oauth')
    else
      ClaudeObj.Add('authMethod', 'api_key');
    
    // Save OAuth tokens if present
    if FClaudeConfig.OAuthAccessToken <> '' then
      ClaudeObj.Add('oauthAccessToken', string(FClaudeConfig.OAuthAccessToken));
    if FClaudeConfig.OAuthRefreshToken <> '' then
      ClaudeObj.Add('oauthRefreshToken', string(FClaudeConfig.OAuthRefreshToken));
    if FClaudeConfig.OAuthTokenType <> '' then
      ClaudeObj.Add('oauthTokenType', string(FClaudeConfig.OAuthTokenType));
    if FClaudeConfig.OAuthExpiresAt <> 0 then
      ClaudeObj.Add('oauthExpiresAt', FClaudeConfig.OAuthExpiresAt);
    if FClaudeConfig.OAuthScope <> '' then
      ClaudeObj.Add('oauthScope', string(FClaudeConfig.OAuthScope));
    
    ProvidersObj.Add('claude', ClaudeObj);
  end;
  
  // Gemini
  if FGeminiConfig.Enabled and ((FGeminiConfig.ApiKey <> '') or (FGeminiConfig.AuthMethod = amOAuth)) then
  begin
    GeminiObj := TJSONObject.Create;
    GeminiObj.Add('enabled', FGeminiConfig.Enabled);
    
    // Only add API key if not using OAuth
    if FGeminiConfig.AuthMethod <> amOAuth then
      GeminiObj.Add('apiKey', string(FGeminiConfig.ApiKey));
    
    GeminiObj.Add('model', string(FGeminiConfig.Model));
    if FGeminiConfig.AuthMethod = amOAuth then
      GeminiObj.Add('authMethod', 'oauth')
    else
      GeminiObj.Add('authMethod', 'api_key');
    ProvidersObj.Add('gemini', GeminiObj);
  end;
  
  Result.Add('providers', ProvidersObj);
end;

procedure TGitPalConfig.SetSecureFilePermissions(const FilePath: AnsiString);
begin
  {$IFDEF UNIX}
  // Set file permissions to 600 (owner read/write only)
  if FpChmod(PChar(FilePath), &600) <> 0 then
  begin
    // Non-fatal error, but we should note it
    WriteLn('Warning: Could not set secure permissions on config file');
  end;
  {$ENDIF}
end;

function TGitPalConfig.LoadConfig: Boolean;
var
  ConfigFile: TextFile;
  JsonText: AnsiString;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Line: AnsiString;
begin
  Result := False;
  
  if not FileExists(string(FConfigPath)) then
    Exit;
  
  try
    // Read the entire file
    JsonText := AnsiString('');
    AssignFile(ConfigFile, string(FConfigPath));
    Reset(ConfigFile);
    try
      while not Eof(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        JsonText := JsonText + Line + #10;
      end;
    finally
      CloseFile(ConfigFile);
    end;
    
    // Parse JSON
    JsonData := GetJSON(string(JsonText));
    try
      if JsonData is TJSONObject then
      begin
        JsonObj := TJSONObject(JsonData);
        Result := LoadFromJSON(JsonObj);
      end;
    finally
      JsonData.Free;
    end;
  except
    // File reading or JSON parsing failed
    Result := False;
  end;
end;

procedure TGitPalConfig.SaveConfig;
var
  ConfigFile: TextFile;
  JsonObj: TJSONObject;
  JsonText: AnsiString;
begin
  if not EnsureConfigDir then
    raise Exception.Create('Could not create configuration directory');
  
  JsonObj := SaveToJSON;
  try
    JsonText := AnsiString(JsonObj.FormatJSON);
    
    // Write to file
    AssignFile(ConfigFile, string(FConfigPath));
    Rewrite(ConfigFile);
    try
      Write(ConfigFile, string(JsonText));
    finally
      CloseFile(ConfigFile);
    end;
    
    // Set secure permissions
    SetSecureFilePermissions(FConfigPath);
  finally
    JsonObj.Free;
  end;
end;

function TGitPalConfig.HasValidConfig: Boolean;
begin
  Result := (FDefaultProvider <> '') and HasProviderApiKey(FDefaultProvider);
end;

function TGitPalConfig.ConfigExists: Boolean;
begin
  Result := FileExists(string(FConfigPath));
end;

procedure TGitPalConfig.ClearConfig;
begin
  if FileExists(string(FConfigPath)) then
  begin
    if not DeleteFile(string(FConfigPath)) then
      raise Exception.Create('Failed to delete configuration file');
  end;
  
  // Reset to defaults
  InitializeDefaults;
end;

function TGitPalConfig.GetProviderConfig(const ProviderName: AnsiString): TProviderConfig;
begin
  if ProviderName = PROVIDER_OPENAI then
    Result := FOpenAIConfig
  else if ProviderName = PROVIDER_CLAUDE then
    Result := FClaudeConfig
  else if ProviderName = PROVIDER_GEMINI then
    Result := FGeminiConfig
  else
  begin
    // Return empty config for unknown provider
    FillChar(Result, SizeOf(Result), 0);
    Result.Enabled := False;
    Result.ApiKey := AnsiString('');
    Result.Model := AnsiString('');
  end;
end;

procedure TGitPalConfig.SetProviderConfig(const ProviderName: AnsiString; const Config: TProviderConfig);
begin
  if ProviderName = PROVIDER_OPENAI then
    FOpenAIConfig := Config
  else if ProviderName = PROVIDER_CLAUDE then
    FClaudeConfig := Config
  else if ProviderName = PROVIDER_GEMINI then
    FGeminiConfig := Config;
  // Ignore unknown providers
end;

function TGitPalConfig.HasProviderApiKey(const ProviderName: AnsiString): Boolean;
var
  Config: TProviderConfig;
begin
  Config := GetProviderConfig(ProviderName);
  // Check for API key OR OAuth tokens
  Result := Config.Enabled and 
    ((Config.ApiKey <> '') or 
     ((Config.AuthMethod = amOAuth) and (Config.OAuthAccessToken <> '')));
end;

function TGitPalConfig.GetProviderApiKey(const ProviderName: AnsiString): AnsiString;
var
  Config: TProviderConfig;
begin
  Config := GetProviderConfig(ProviderName);
  if Config.Enabled then
    Result := Config.ApiKey
  else
    Result := AnsiString('');
end;

function TGitPalConfig.DetectEnvironmentApiKey(const ProviderName: AnsiString): AnsiString;
var
  EnvVar: AnsiString;
begin
  if ProviderName = PROVIDER_OPENAI then
    EnvVar := AnsiString(ENV_OPENAI_API_KEY)
  else if ProviderName = PROVIDER_CLAUDE then
    EnvVar := AnsiString(ENV_ANTHROPIC_API_KEY)
  else if ProviderName = PROVIDER_GEMINI then
    EnvVar := AnsiString(ENV_GEMINI_API_KEY)
  else
    EnvVar := AnsiString('');
  
  if EnvVar <> '' then
    Result := AnsiString(GetEnvironmentVariable(string(EnvVar)))
  else
    Result := AnsiString('');
end;

end.