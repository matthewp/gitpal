unit command_setup;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  bobaui,
  bobastyle,
  bobacomponents,
  SysUtils,
  Classes,
  config_manager,
  provider_config;

type
  TSetupWizardState = (
    swWelcome,
    swProviderSelection,
    swApiKeyInput,
    swModelSelection,
    swSummary,
    swCompleted
  );
  
  TSetupModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: Integer;
    FState: TSetupWizardState;
    FConfig: TGitPalConfig;
    FRegistry: TProviderRegistry;
    FSelectedProvider: AnsiString;
    FProviderNames: TStringList;
    FDisplayNames: TStringList;
    FAvailableModels: TStringList;
    FSelectedProviderIndex: Integer;
    FSelectedModelIndex: Integer;
    FApiKeyInput: AnsiString;
    FDetectedApiKey: AnsiString;
    FCursor: Boolean;
    FErrorMessage: AnsiString;
    
    // State management
    procedure InitializeProviderLists;
    procedure DetectEnvironmentApiKey;
    function ValidateCurrentInput: Boolean;
    procedure SaveConfiguration;
    
    // View helpers
    function RenderWelcome: string;
    function RenderProviderSelection: string;
    function RenderApiKeyInput: string;
    function RenderModelSelection: string;
    function RenderSummary: string;
    function RenderCompleted: string;
    function RenderError(const Message: AnsiString): string;
    
    // Navigation
    procedure NextState;
    procedure PreviousState;
    
  public
    constructor Create;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

procedure RunSetupWizard;

implementation

constructor TSetupModel.Create;
begin
  inherited Create;
  FTerminalWidth := 0;  // Will be set by WindowSizeMsg
  FTerminalHeight := 0; // Will be set by WindowSizeMsg
  FState := swWelcome;
  FConfig := TGitPalConfig.Create;
  FRegistry := TProviderRegistry.Create;
  FSelectedProviderIndex := 0;
  FSelectedModelIndex := 0;
  FApiKeyInput := AnsiString('');
  FDetectedApiKey := AnsiString('');
  FCursor := True;
  FErrorMessage := AnsiString('');
  
  InitializeProviderLists;
end;

destructor TSetupModel.Destroy;
begin
  if Assigned(FProviderNames) then FProviderNames.Free;
  if Assigned(FDisplayNames) then FDisplayNames.Free;
  if Assigned(FAvailableModels) then FAvailableModels.Free;
  FRegistry.Free;
  FConfig.Free;
  inherited Destroy;
end;

procedure TSetupModel.InitializeProviderLists;
begin
  FProviderNames := FRegistry.GetProviderNames;
  FDisplayNames := FRegistry.GetProviderDisplayNames;
  if FProviderNames.Count > 0 then
    FSelectedProvider := AnsiString(FProviderNames[0]);
end;

procedure TSetupModel.DetectEnvironmentApiKey;
begin
  if FSelectedProvider <> '' then
    FDetectedApiKey := FConfig.DetectEnvironmentApiKey(FSelectedProvider);
end;

function TSetupModel.ValidateCurrentInput: Boolean;
begin
  Result := True;
  FErrorMessage := AnsiString('');
  
  case FState of
    swApiKeyInput:
      begin
        if (FApiKeyInput = '') and (FDetectedApiKey = '') then
        begin
          FErrorMessage := AnsiString('API key is required');
          Result := False;
        end
        else if (FApiKeyInput <> '') and (not FRegistry.ValidateApiKey(FSelectedProvider, FApiKeyInput)) then
        begin
          FErrorMessage := AnsiString('Invalid API key format');
          Result := False;
        end;
      end;
  end;
end;

procedure TSetupModel.SaveConfiguration;
var
  ProviderConfig: TProviderConfig;
  ApiKeyToUse: AnsiString;
begin
  // Determine which API key to use
  if FApiKeyInput <> '' then
    ApiKeyToUse := FApiKeyInput
  else
    ApiKeyToUse := FDetectedApiKey;
  
  // Configure the selected provider
  ProviderConfig.Enabled := True;
  ProviderConfig.ApiKey := ApiKeyToUse;
  if Assigned(FAvailableModels) and (FSelectedModelIndex < FAvailableModels.Count) then
    ProviderConfig.Model := AnsiString(FAvailableModels[FSelectedModelIndex])
  else
    ProviderConfig.Model := FRegistry.GetDefaultModelForProvider(FSelectedProvider);
  
  FConfig.SetProviderConfig(FSelectedProvider, ProviderConfig);
  FConfig.DefaultProvider := FSelectedProvider;
  
  // Save to disk
  FConfig.SaveConfig;
end;

function TSetupModel.RenderWelcome: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
begin
  Content := 
    AnsiString('Welcome to gitpal setup!') + #10 + #10 +
    AnsiString('This wizard will help you configure gitpal to use AI for generating') + #10 +
    AnsiString('commit messages. You can choose between OpenAI, Claude, or Gemini.') + #10 + #10 +
    AnsiString('Press ENTER to continue or ESC to exit.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightBlue;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TSetupModel.RenderProviderSelection: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
  I: Integer;
  ProviderInfo: TProviderInfo;
begin
  Content := AnsiString('Select your AI provider:') + #10 + #10;
  
  for I := 0 to FDisplayNames.Count - 1 do
  begin
    ProviderInfo := FRegistry.GetProviderInfo(AnsiString(FProviderNames[I]));
    if I = FSelectedProviderIndex then
      Content := Content + AnsiString('> ') + AnsiString(FDisplayNames[I]) + #10 + AnsiString('  ') + ProviderInfo.Description + #10 + #10
    else
      Content := Content + AnsiString('  ') + AnsiString(FDisplayNames[I]) + #10 + AnsiString('  ') + ProviderInfo.Description + #10 + #10;
  end;
  
  Content := Content + AnsiString('Use ↑/↓ (or k/j) to select, ENTER to confirm, ESC to go back.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightGreen;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TSetupModel.RenderApiKeyInput: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
  ProviderInfo: TProviderInfo;
  MaskedKey: AnsiString;
begin
  ProviderInfo := FRegistry.GetProviderInfo(FSelectedProvider);
  Content := AnsiString('Configure ') + ProviderInfo.DisplayName + AnsiString(' API Key') + #10 + #10;
  
  if FDetectedApiKey <> '' then
  begin
    // Show detected API key (masked)
    MaskedKey := Copy(FDetectedApiKey, 1, 8) + '...' + Copy(FDetectedApiKey, Length(FDetectedApiKey) - 3, 4);
    Content := Content + 
      AnsiString('Found ') + ProviderInfo.EnvVarName + AnsiString(': ') + MaskedKey + #10 +
      AnsiString('Press ENTER to use this key, or type a new one:') + #10 + #10;
  end
  else
  begin
    Content := Content + 
      AnsiString('Enter your ') + ProviderInfo.DisplayName + AnsiString(' API key:') + #10 +
      AnsiString('(Set ') + ProviderInfo.EnvVarName + AnsiString(' environment variable or enter manually)') + #10 + #10;
  end;
  
  if FApiKeyInput <> '' then
  begin
    // Show masked input
    MaskedKey := Copy(FApiKeyInput, 1, 8) + AnsiString(StringOfChar('*', Length(FApiKeyInput) - 8));
    Content := Content + AnsiString('> ') + MaskedKey;
    if FCursor then Content := Content + AnsiString('|');
  end
  else
  begin
    Content := Content + AnsiString('> ');
    if FCursor then Content := Content + AnsiString('|');
  end;
  
  Content := Content + #10 + #10 + AnsiString('ENTER to continue, ESC to go back.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightYellow;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TSetupModel.RenderModelSelection: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
  I: Integer;
  ProviderInfo: TProviderInfo;
begin
  ProviderInfo := FRegistry.GetProviderInfo(FSelectedProvider);
  Content := AnsiString('Select model for ') + ProviderInfo.DisplayName + AnsiString(':') + #10 + #10;
  
  for I := 0 to FAvailableModels.Count - 1 do
  begin
    if I = FSelectedModelIndex then
      Content := Content + AnsiString('> ') + AnsiString(FAvailableModels[I]) + #10
    else
      Content := Content + AnsiString('  ') + AnsiString(FAvailableModels[I]) + #10;
  end;
  
  Content := Content + #10 + AnsiString('Use ↑/↓ (or k/j) to select, ENTER to confirm, ESC to go back.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightMagenta;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;


function TSetupModel.RenderSummary: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
  ProviderInfo: TProviderInfo;
  ModelName: AnsiString;
  ApiKeyDisplay: AnsiString;
begin
  ProviderInfo := FRegistry.GetProviderInfo(FSelectedProvider);
  
  if Assigned(FAvailableModels) and (FSelectedModelIndex < FAvailableModels.Count) then
    ModelName := AnsiString(FAvailableModels[FSelectedModelIndex])
  else
    ModelName := FRegistry.GetDefaultModelForProvider(FSelectedProvider);
  
  if FApiKeyInput <> '' then
    ApiKeyDisplay := Copy(FApiKeyInput, 1, 8) + AnsiString('...(manual)')
  else
    ApiKeyDisplay := Copy(FDetectedApiKey, 1, 8) + AnsiString('...(from env)');
  
  Content := 
    AnsiString('Configuration Summary:') + #10 + #10 +
    AnsiString('Provider: ') + ProviderInfo.DisplayName + #10 +
    AnsiString('Model: ') + ModelName + #10 +
    AnsiString('API Key: ') + ApiKeyDisplay + #10 +
    AnsiString('Config: ~/.config/gitpal/config.json') + #10 + #10 +
    AnsiString('Press ENTER to save and complete setup, ESC to go back.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightGreen;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TSetupModel.RenderCompleted: string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
begin
  Content := 
    AnsiString('Setup completed successfully!') + #10 + #10 +
    AnsiString('You can now use gitpal to generate commit messages:') + #10 +
    AnsiString('  gitpal commit') + #10 + #10 +
    AnsiString('To reconfigure, run:') + #10 +
    AnsiString('  gitpal setup') + #10 + #10 +
    AnsiString('Press any key to exit.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsDouble;
    Style.BorderColor := bobastyle.cBrightGreen;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TSetupModel.RenderError(const Message: AnsiString): string;
var
  Style: bobastyle.TStyle;
  Content: AnsiString;
  PaddedContent: AnsiString;
begin
  Content := AnsiString('Error: ') + Message + #10 + #10 + AnsiString('Press any key to continue.');
  
  PaddedContent := AnsiString(' ') + Content + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightRed;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

procedure TSetupModel.NextState;
begin
  case FState of
    swWelcome: FState := swProviderSelection;
    swProviderSelection:
      begin
        FSelectedProvider := AnsiString(FProviderNames[FSelectedProviderIndex]);
        DetectEnvironmentApiKey;
        FState := swApiKeyInput;
      end;
    swApiKeyInput:
      begin
        if ValidateCurrentInput then
        begin
          if Assigned(FAvailableModels) then FAvailableModels.Free;
          FAvailableModels := FRegistry.GetAvailableModels(FSelectedProvider);
          FSelectedModelIndex := 0;
          FState := swModelSelection;
        end;
      end;
    swModelSelection: FState := swSummary;
    swSummary:
      begin
        try
          SaveConfiguration;
          FState := swCompleted;
        except
          on E: Exception do
          begin
            FErrorMessage := AnsiString('Failed to save configuration: ' + E.Message);
          end;
        end;
      end;
  end;
end;

procedure TSetupModel.PreviousState;
begin
  case FState of
    swProviderSelection: FState := swWelcome;
    swApiKeyInput:
      begin
        FApiKeyInput := AnsiString('');
        FDetectedApiKey := AnsiString('');
        FState := swProviderSelection;
      end;
    swModelSelection: FState := swApiKeyInput;
    swSummary: FState := swModelSelection;
  end;
end;

function TSetupModel.View: string;
begin
  if FTerminalWidth <= 0 then
  begin
    Result := 'Initializing...';
    Exit;
  end;
  
  if FErrorMessage <> '' then
  begin
    Result := RenderError(FErrorMessage);
    Exit;
  end;
  
  case FState of
    swWelcome: Result := RenderWelcome;
    swProviderSelection: Result := RenderProviderSelection;
    swApiKeyInput: Result := RenderApiKeyInput;
    swModelSelection: Result := RenderModelSelection;
    swSummary: Result := RenderSummary;
    swCompleted: Result := RenderCompleted;
    else Result := 'Unknown state';
  end;
end;

function TSetupModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  WindowSizeMsg: bobaui.TWindowSizeMsg;
  KeyMsg: bobaui.TKeyMsg;
begin
  Result.Model := Self;
  Result.Cmd := nil;
  
  if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowSizeMsg := bobaui.TWindowSizeMsg(Msg);
    FTerminalWidth := WindowSizeMsg.Width;
    FTerminalHeight := WindowSizeMsg.Height;
  end
  else if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Clear error message on any key press
    if FErrorMessage <> '' then
    begin
      FErrorMessage := AnsiString('');
      Exit;
    end;
    
    if KeyMsg.Key = #27 then // Escape
    begin
      if FState = swWelcome then
        Result.Cmd := bobaui.QuitCmd
      else
        PreviousState;
    end
    else if (KeyMsg.Key = 'q') or (KeyMsg.Key = 'Q') then // Q to quit anytime
    begin
      Result.Cmd := bobaui.QuitCmd;
    end
    else if (KeyMsg.Key = #13) or (KeyMsg.Key = #10) or (KeyMsg.Key = ' ') then // Enter (CR or LF) or Space
    begin
      if FState = swCompleted then
        Result.Cmd := bobaui.QuitCmd
      else
        NextState;
    end
    else if KeyMsg.IsUpArrow or (KeyMsg.Key = 'k') or (KeyMsg.Key = 'K') then
    begin
      case FState of
        swProviderSelection:
          if FSelectedProviderIndex > 0 then
            Dec(FSelectedProviderIndex);
        swModelSelection:
          if FSelectedModelIndex > 0 then
            Dec(FSelectedModelIndex);
      end;
    end
    else if KeyMsg.IsDownArrow or (KeyMsg.Key = 'j') or (KeyMsg.Key = 'J') then
    begin
      case FState of
        swProviderSelection:
          if FSelectedProviderIndex < FProviderNames.Count - 1 then
            Inc(FSelectedProviderIndex);
        swModelSelection:
          if Assigned(FAvailableModels) and (FSelectedModelIndex < FAvailableModels.Count - 1) then
            Inc(FSelectedModelIndex);
      end;
    end
    else if KeyMsg.Key = #8 then // Backspace
    begin
      if (FState = swApiKeyInput) and (Length(FApiKeyInput) > 0) then
        FApiKeyInput := Copy(FApiKeyInput, 1, Length(FApiKeyInput) - 1);
    end
    else if (FState = swApiKeyInput) and (Ord(KeyMsg.Key) >= 32) and (Ord(KeyMsg.Key) <= 126) then
    begin
      // Handle text input for API key
      FApiKeyInput := FApiKeyInput + AnsiChar(KeyMsg.Key);
    end
    else if FState = swCompleted then
    begin
      // Any key exits from completed state
      Result.Cmd := bobaui.QuitCmd;
    end;
  end;
end;

procedure RunSetupWizard;
var
  Prog: TBobaUIProgram;
  Model: TSetupModel;
begin
  Model := TSetupModel.Create;
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  try
    Prog.Run;
  finally
    Prog.Free;
  end;
end;

end.
