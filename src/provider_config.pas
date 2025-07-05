{$mode objfpc}
{$codepage UTF8}
{$H+}

unit provider_config;

interface

uses
  SysUtils, Classes, models, openai_provider, claude_provider, gemini_provider, 
  claude_oauth_provider, config_manager;

type
  TProviderInfo = record
    Name: AnsiString;                    // Internal provider name
    DisplayName: AnsiString;             // User-friendly display name
    Description: AnsiString;             // Description for setup wizard
    DefaultModel: AnsiString;            // Default model for this provider
    AvailableModels: array of AnsiString; // List of available models
    EnvVarName: AnsiString;              // Environment variable name for API key
  end;

  TProviderRegistry = class
  private
    FProviders: array of TProviderInfo;
    procedure InitializeProviders;
    function GetProviderIndex(const ProviderName: AnsiString): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Provider information
    function GetProviderInfo(const ProviderName: AnsiString): TProviderInfo;
    function GetProviderNames: TStringList;
    function GetProviderDisplayNames: TStringList;
    function IsValidProvider(const ProviderName: AnsiString): Boolean;
    function IsValidModel(const ProviderName, ModelName: AnsiString): Boolean;
    
    // Provider factory methods
    function CreateProvider(const ProviderName, ApiKey, Model: AnsiString): ILLMProvider;
    function CreateProviderFromConfig(Config: TGitPalConfig; const ProviderName: AnsiString): ILLMProvider;
    function CreateDefaultProvider(Config: TGitPalConfig): ILLMProvider;
    
    // Validation helpers
    function ValidateApiKey(const ProviderName, ApiKey: AnsiString): Boolean;
    function GetDefaultModelForProvider(const ProviderName: AnsiString): AnsiString;
    function GetAvailableModels(const ProviderName: AnsiString): TStringList;
  end;

implementation

constructor TProviderRegistry.Create;
begin
  inherited Create;
  InitializeProviders;
end;

destructor TProviderRegistry.Destroy;
begin
  inherited Destroy;
end;

procedure TProviderRegistry.InitializeProviders;
begin
  SetLength(FProviders, 3);
  
  // OpenAI Configuration
  FProviders[0].Name := PROVIDER_OPENAI;
  FProviders[0].DisplayName := 'OpenAI';
  FProviders[0].Description := 'OpenAI GPT models (GPT-4o, GPT-4o-mini)';
  FProviders[0].DefaultModel := 'gpt-4o';
  SetLength(FProviders[0].AvailableModels, 4);
  FProviders[0].AvailableModels[0] := 'gpt-4o';
  FProviders[0].AvailableModels[1] := 'gpt-4o-mini';
  FProviders[0].AvailableModels[2] := 'gpt-4-turbo';
  FProviders[0].AvailableModels[3] := 'gpt-3.5-turbo';
  FProviders[0].EnvVarName := ENV_OPENAI_API_KEY;
  
  // Claude/Anthropic Configuration
  FProviders[1].Name := PROVIDER_CLAUDE;
  FProviders[1].DisplayName := 'Claude (Anthropic)';
  FProviders[1].Description := 'Anthropic Claude models (Claude 3.5 Sonnet, Claude 3.5 Haiku)';
  FProviders[1].DefaultModel := 'claude-3-5-sonnet-latest';
  SetLength(FProviders[1].AvailableModels, 4);
  FProviders[1].AvailableModels[0] := 'claude-3-5-sonnet-latest';
  FProviders[1].AvailableModels[1] := 'claude-3-5-haiku-latest';
  FProviders[1].AvailableModels[2] := 'claude-3-5-sonnet-20241022';
  FProviders[1].AvailableModels[3] := 'claude-3-5-haiku-20241022';
  FProviders[1].EnvVarName := ENV_ANTHROPIC_API_KEY;
  
  // Gemini Configuration
  FProviders[2].Name := PROVIDER_GEMINI;
  FProviders[2].DisplayName := 'Google Gemini';
  FProviders[2].Description := 'Google Gemini models (Gemini 2.0 Flash, Gemini 1.5 Pro)';
  FProviders[2].DefaultModel := 'gemini-2.0-flash-exp';
  SetLength(FProviders[2].AvailableModels, 4);
  FProviders[2].AvailableModels[0] := 'gemini-2.0-flash-exp';
  FProviders[2].AvailableModels[1] := 'gemini-1.5-flash';
  FProviders[2].AvailableModels[2] := 'gemini-1.5-pro';
  FProviders[2].AvailableModels[3] := 'gemini-1.5-flash-8b';
  FProviders[2].EnvVarName := ENV_GEMINI_API_KEY;
end;

function TProviderRegistry.GetProviderIndex(const ProviderName: AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FProviders) do
  begin
    if FProviders[I].Name = ProviderName then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TProviderRegistry.GetProviderInfo(const ProviderName: AnsiString): TProviderInfo;
var
  Index: Integer;
begin
  Index := GetProviderIndex(ProviderName);
  if Index >= 0 then
    Result := FProviders[Index]
  else
  begin
    // Return empty info for unknown provider
    FillChar(Result, SizeOf(Result), 0);
    Result.Name := ProviderName;
    Result.DisplayName := 'Unknown Provider';
    Result.Description := 'Unknown provider';
    Result.DefaultModel := '';
    SetLength(Result.AvailableModels, 0);
    Result.EnvVarName := '';
  end;
end;

function TProviderRegistry.GetProviderNames: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to High(FProviders) do
    Result.Add(string(FProviders[I].Name));
end;

function TProviderRegistry.GetProviderDisplayNames: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to High(FProviders) do
    Result.Add(string(FProviders[I].DisplayName));
end;

function TProviderRegistry.IsValidProvider(const ProviderName: AnsiString): Boolean;
begin
  Result := GetProviderIndex(ProviderName) >= 0;
end;

function TProviderRegistry.IsValidModel(const ProviderName, ModelName: AnsiString): Boolean;
var
  Info: TProviderInfo;
  I: Integer;
begin
  Result := False;
  if not IsValidProvider(ProviderName) then
    Exit;
    
  Info := GetProviderInfo(ProviderName);
  for I := 0 to High(Info.AvailableModels) do
  begin
    if Info.AvailableModels[I] = ModelName then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TProviderRegistry.CreateProvider(const ProviderName, ApiKey, Model: AnsiString): ILLMProvider;
var
  ModelToUse: AnsiString;
begin
  Result := nil;
  
  if not IsValidProvider(ProviderName) then
    raise Exception.CreateFmt('Unknown provider: %s', [string(ProviderName)]);
  
  // API key required for all providers (OAuth is handled in CreateProviderFromConfig)
  if ApiKey = '' then
    raise Exception.CreateFmt('API key required for provider: %s', [string(ProviderName)]);
  
  // Use provided model or fall back to default
  ModelToUse := Model;
  if (ModelToUse = '') or (not IsValidModel(ProviderName, ModelToUse)) then
    ModelToUse := GetDefaultModelForProvider(ProviderName);
  
  // Create the appropriate provider instance
  if ProviderName = PROVIDER_OPENAI then
    Result := TOpenAIProvider.Create(string(ApiKey))
  else if ProviderName = PROVIDER_CLAUDE then
    Result := TClaudeProvider.Create(string(ApiKey))
  else if ProviderName = PROVIDER_GEMINI then
    Result := TGeminiProvider.Create(string(ApiKey))
  else
    raise Exception.CreateFmt('Provider not implemented: %s', [string(ProviderName)]);
end;

function TProviderRegistry.CreateProviderFromConfig(Config: TGitPalConfig; const ProviderName: AnsiString): ILLMProvider;
var
  ProviderConfig: TProviderConfig;
  ApiKey: AnsiString;
begin
  ProviderConfig := Config.GetProviderConfig(ProviderName);
  
  // Handle Claude OAuth case specially
  if (ProviderName = PROVIDER_CLAUDE) and (ProviderConfig.AuthMethod = amOAuth) then
  begin
    Result := TClaudeOAuthProvider.Create;
    Exit;
  end;
  
  // For API key providers, validate API key exists
  if not Config.HasProviderApiKey(ProviderName) then
    raise Exception.CreateFmt('No API key configured for provider: %s', [string(ProviderName)]);
  
  ApiKey := ProviderConfig.ApiKey;
  
  Result := CreateProvider(ProviderName, ApiKey, ProviderConfig.Model);
end;

function TProviderRegistry.CreateDefaultProvider(Config: TGitPalConfig): ILLMProvider;
var
  ProviderConfig: TProviderConfig;
begin
  if Config.DefaultProvider = '' then
    raise Exception.Create('No default provider configured');
  
  ProviderConfig := Config.GetProviderConfig(Config.DefaultProvider);
  
  // Check for proper authentication
  if ProviderConfig.AuthMethod = amOAuth then
  begin
    if ProviderConfig.OAuthAccessToken = '' then
      raise Exception.CreateFmt('Default provider %s has no OAuth tokens configured', [string(Config.DefaultProvider)]);
  end
  else
  begin
    if not Config.HasProviderApiKey(Config.DefaultProvider) then
      raise Exception.CreateFmt('Default provider %s has no API key configured', [string(Config.DefaultProvider)]);
  end;
  
  Result := CreateProviderFromConfig(Config, Config.DefaultProvider);
end;

function TProviderRegistry.ValidateApiKey(const ProviderName, ApiKey: AnsiString): Boolean;
begin
  // Basic validation - check format and length
  Result := False;
  
  if ApiKey = '' then
    Exit;
  
  if ProviderName = PROVIDER_OPENAI then
  begin
    // OpenAI keys start with 'sk-' and are typically 51+ characters
    Result := (Length(ApiKey) >= 20) and (Copy(ApiKey, 1, 3) = 'sk-');
  end
  else if ProviderName = PROVIDER_CLAUDE then
  begin
    // Anthropic keys start with 'sk-ant-' and are typically 100+ characters
    Result := (Length(ApiKey) >= 20) and (Copy(ApiKey, 1, 7) = 'sk-ant-');
  end
  else if ProviderName = PROVIDER_GEMINI then
  begin
    // Gemini keys start with 'AIza' and are typically 39 characters
    Result := (Length(ApiKey) >= 20) and (Copy(ApiKey, 1, 4) = 'AIza');
  end;
end;

function TProviderRegistry.GetDefaultModelForProvider(const ProviderName: AnsiString): AnsiString;
var
  Info: TProviderInfo;
begin
  Info := GetProviderInfo(ProviderName);
  Result := Info.DefaultModel;
end;

function TProviderRegistry.GetAvailableModels(const ProviderName: AnsiString): TStringList;
var
  Info: TProviderInfo;
  I: Integer;
begin
  Result := TStringList.Create;
  Info := GetProviderInfo(ProviderName);
  
  for I := 0 to High(Info.AvailableModels) do
    Result.Add(string(Info.AvailableModels[I]));
end;

end.