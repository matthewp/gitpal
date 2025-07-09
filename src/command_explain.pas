unit command_explain;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  bobaui,
  bobastyle,
  bobacomponents,
  git,
  models,
  config_manager,
  provider_config,
  logging,
  SysUtils,
  Classes,
  DateUtils,
  Math;

type
  // Reflog entry data structure
  TReflogEntry = record
    Hash: string;
    ShortHash: string;
    Operation: string;
    Subject: string;
    Date: TDateTime;
    Author: string;
    RefName: string;
  end;

  TReflogEntryArray = array of TReflogEntry;

  // Operation analysis data structure
  TOperationAnalysis = record
    OperationType: string;
    TimeRange: string;
    BeforeState: string;
    AfterState: string;
    Changes: array of string;
    Impact: string;
    Explanation: string;
  end;

  // Progress stages for explain operation
  TExplainProgressStage = (
    esInitializing,          // Starting up
    esLoadingReflog,         // Reading git reflog
    esAnalyzingOperations,   // Analyzing operations with AI
    esCompleted,             // Analysis complete
    esError                  // Error occurred
  );

  // Main explain model
  TExplainModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FSpinner: bobacomponents.TSpinner;
    FProviderOverride: string;
    FCurrentStage: TExplainProgressStage;
    FStatusMessage: string;
    FReflogEntries: TReflogEntryArray;
    FOperationAnalysis: TOperationAnalysis;
    FHasAnalysis: Boolean;
    FErrorMessage: string;
    
    procedure StartExplainAnalysis;
    procedure LoadReflogEntries;
    procedure AnalyzeOperationsWithAI;
    function FormatAnalysisView: string;
  public
    constructor Create; overload;
    constructor Create(const AProviderOverride: string); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

// Public functions
procedure RunExplainCommand(const ProviderOverride: string = '');

implementation

// Forward declarations
function ParseReflogEntries(const ReflogOutput: string): TReflogEntryArray; forward;
function GenerateOperationAnalysis(const Entries: TReflogEntryArray; const ProviderOverride: string): TOperationAnalysis; forward;

constructor TExplainModel.Create;
begin
  inherited Create;
  FTerminalWidth := 0;
  FTerminalHeight := 0;
  FProviderOverride := AnsiString('');
  FCurrentStage := esInitializing;
  FStatusMessage := AnsiString('Initializing...');
  FHasAnalysis := False;
  FErrorMessage := AnsiString('');
  
  SetLength(FReflogEntries, 0);
  FillChar(FOperationAnalysis, SizeOf(FOperationAnalysis), 0);
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;

constructor TExplainModel.Create(const AProviderOverride: string);
begin
  Create;
  FProviderOverride := AProviderOverride;
end;

destructor TExplainModel.Destroy;
begin
  try
    if Assigned(FSpinner) then
    begin
      FSpinner.Free;
      FSpinner := nil;
    end;
    inherited Destroy;
  except
    // Silently ignore cleanup exceptions
  end;
end;

procedure TExplainModel.StartExplainAnalysis;
begin
  FCurrentStage := esLoadingReflog;
  FStatusMessage := 'Reading git reflog...';
  
  try
    LoadReflogEntries;
    
    if Length(FReflogEntries) = 0 then
    begin
      FCurrentStage := esError;
      FErrorMessage := 'No reflog entries found. Repository may not have any operations recorded.';
      Exit;
    end;
    
    FCurrentStage := esAnalyzingOperations;
    FStatusMessage := 'Analyzing operations with AI...';
    
    AnalyzeOperationsWithAI;
    
    FCurrentStage := esCompleted;
    FStatusMessage := 'Analysis complete';
    
  except
    on E: Exception do
    begin
      FCurrentStage := esError;
      FErrorMessage := 'Error during analysis: ' + E.Message;
      DebugLog('[TExplainModel.StartExplainAnalysis] Exception: ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TExplainModel.LoadReflogEntries;
var
  ReflogResult: git.TGitResult;
begin
  DebugLog('[TExplainModel.LoadReflogEntries] Loading reflog entries');
  
  // Get recent reflog entries with detailed format
  ReflogResult := git.TGitRepository.GetReflog([
    '--all',
    '--format=%H|%h|%gD|%gs|%ai|%an|%gn',
    '-n',
    '20'
  ]);
  
  if not ReflogResult.Success then
  begin
    raise Exception.Create('Failed to get reflog: ' + ReflogResult.ErrorMessage);
  end;
  
  FReflogEntries := ParseReflogEntries(ReflogResult.Output);
  DebugLog('[TExplainModel.LoadReflogEntries] Loaded ' + IntToStr(Length(FReflogEntries)) + ' entries');
end;

procedure TExplainModel.AnalyzeOperationsWithAI;
begin
  DebugLog('[TExplainModel.AnalyzeOperationsWithAI] Starting AI analysis');
  
  FOperationAnalysis := GenerateOperationAnalysis(FReflogEntries, FProviderOverride);
  FHasAnalysis := True;
  
  DebugLog('[TExplainModel.AnalyzeOperationsWithAI] Analysis completed');
end;

function TExplainModel.FormatAnalysisView: string;
var
  Sections: array of string;
  i: Integer;
begin
  SetLength(Sections, 0);
  
  // Title
  SetLength(Sections, Length(Sections) + 1);
  Sections[High(Sections)] := bobastyle.ColorText(AnsiString('📊 Repository Operation Analysis'), bobastyle.cBrightBlue);
  
  // Time range
  if FOperationAnalysis.TimeRange <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('🕐 Time Range: '), bobastyle.cBrightWhite) + FOperationAnalysis.TimeRange;
  end;
  
  // Operation type
  if FOperationAnalysis.OperationType <> '' then
  begin
    SetLength(Sections, Length(Sections) + 1);
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('⚡ Operation Type: '), bobastyle.cBrightWhite) + FOperationAnalysis.OperationType;
  end;
  
  // Before/After states
  if FOperationAnalysis.BeforeState <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('📈 Before: '), bobastyle.cBrightWhite) + FOperationAnalysis.BeforeState;
  end;
  
  if FOperationAnalysis.AfterState <> '' then
  begin
    SetLength(Sections, Length(Sections) + 1);
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('📉 After: '), bobastyle.cBrightWhite) + FOperationAnalysis.AfterState;
  end;
  
  // Changes
  if Length(FOperationAnalysis.Changes) > 0 then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('🔄 Changes:'), bobastyle.cBrightWhite);
    
    for i := 0 to High(FOperationAnalysis.Changes) do
    begin
      SetLength(Sections, Length(Sections) + 1);
      Sections[High(Sections)] := AnsiString('• ') + FOperationAnalysis.Changes[i];
    end;
  end;
  
  // Impact
  if FOperationAnalysis.Impact <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('💥 Impact: '), bobastyle.cBrightYellow) + FOperationAnalysis.Impact;
  end;
  
  // Explanation
  if FOperationAnalysis.Explanation <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText(AnsiString('💡 Explanation:'), bobastyle.cBrightGreen);
    SetLength(Sections, Length(Sections) + 1);
    Sections[High(Sections)] := FOperationAnalysis.Explanation;
  end;
  
  // Footer
  SetLength(Sections, Length(Sections) + 2);
  Sections[High(Sections) - 1] := AnsiString('');
  Sections[High(Sections)] := bobastyle.ColorText('Press q to quit', bobastyle.cBrightBlack);
  
  Result := bobastyle.JoinVertical(Sections);
end;

function TExplainModel.View: string;
begin
  // Handle initial state gracefully when terminal size is unknown
  if FTerminalWidth <= 0 then
  begin
    Result := AnsiString('Detecting terminal size...');
    Exit;
  end;
  
  // Show progress during analysis
  if FCurrentStage in [esInitializing, esLoadingReflog, esAnalyzingOperations] then
  begin
    Result := FSpinner.View + AnsiString(' ') + FStatusMessage;
    Exit;
  end;
  
  // Show error state
  if FCurrentStage = esError then
  begin
    Result := bobastyle.ColorText(AnsiString('✗ ') + FErrorMessage, bobastyle.cBrightRed);
    Exit;
  end;
  
  // Show analysis results
  if FHasAnalysis then
  begin
    Result := FormatAnalysisView;
  end
  else
  begin
    Result := bobastyle.ColorText('No analysis available', bobastyle.cBrightRed);
  end;
end;

function TExplainModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  NewSpinner: bobacomponents.TSpinner;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Handle 'q' to quit
    if (KeyMsg.Key = 'q') or (KeyMsg.Key = 'Q') then
    begin
      Result.Cmd := bobaui.QuitCmd;
    end;
  end
  else if Msg is bobaui.TComponentTickMsg then
  begin
    if FCurrentStage in [esInitializing, esLoadingReflog, esAnalyzingOperations] then
    begin
      NewSpinner := FSpinner.Update(Msg);
      if NewSpinner <> FSpinner then
      begin
        FSpinner.Free;
        FSpinner := NewSpinner;
        Result.Cmd := FSpinner.Tick; // Continue animation
      end;
    end;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    FTerminalWidth := WindowMsg.Width;
    FTerminalHeight := WindowMsg.Height;
    
    // Start analysis on first window size message
    if FCurrentStage = esInitializing then
    begin
      StartExplainAnalysis;
      Result.Cmd := bobaui.BatchCmd([bobaui.HideCursorCmd, FSpinner.Tick]);
    end;
  end;
end;

function ParseReflogEntries(const ReflogOutput: string): TReflogEntryArray;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  Parts: TStringList;
  Entry: TReflogEntry;
  EntryList: array of TReflogEntry;
  EntryCount: Integer;
begin
  SetLength(Result, 0);
  EntryCount := 0;
  SetLength(EntryList, 50); // Pre-allocate space
  
  Lines := TStringList.Create;
  Parts := TStringList.Create;
  try
    Lines.Text := ReflogOutput;
    Parts.Delimiter := '|';
    Parts.StrictDelimiter := True;
    
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then Continue;
      
      Parts.DelimitedText := Line;
      if Parts.Count >= 6 then
      begin
        FillChar(Entry, SizeOf(Entry), 0);
        Entry.Hash := Parts[0];
        Entry.ShortHash := Parts[1];
        Entry.RefName := Parts[2];
        Entry.Subject := Parts[3];
        // Parse date from Parts[4] - for now just use current time
        Entry.Date := Now;
        Entry.Author := Parts[5];
        
        // Extract operation type from subject
        if Pos('rebase', LowerCase(Entry.Subject)) > 0 then
          Entry.Operation := 'rebase'
        else if Pos('merge', LowerCase(Entry.Subject)) > 0 then
          Entry.Operation := 'merge'
        else if Pos('reset', LowerCase(Entry.Subject)) > 0 then
          Entry.Operation := 'reset'
        else if Pos('checkout', LowerCase(Entry.Subject)) > 0 then
          Entry.Operation := 'checkout'
        else if Pos('commit', LowerCase(Entry.Subject)) > 0 then
          Entry.Operation := 'commit'
        else
          Entry.Operation := 'other';
        
        // Add to array
        if EntryCount >= Length(EntryList) then
          SetLength(EntryList, Length(EntryList) * 2);
        EntryList[EntryCount] := Entry;
        Inc(EntryCount);
      end;
    end;
    
    // Copy to result array
    SetLength(Result, EntryCount);
    for i := 0 to EntryCount - 1 do
      Result[i] := EntryList[i];
      
  finally
    Lines.Free;
    Parts.Free;
  end;
end;

function GenerateOperationAnalysis(const Entries: TReflogEntryArray; const ProviderOverride: string): TOperationAnalysis;
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  SystemPrompt: string;
  UserPrompt: string;
  ReflogSummary: string;
  i: Integer;
begin
  DebugLog('[GenerateOperationAnalysis] Starting AI analysis of ' + IntToStr(Length(Entries)) + ' reflog entries');
  
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  
  // For now, create a simple analysis without AI
  if Length(Entries) = 0 then
  begin
    Result.OperationType := 'No operations found';
    Result.Explanation := 'No recent git operations were found in the reflog.';
    Exit;
  end;
  
  // Analyze the most recent operations
  Result.OperationType := 'Recent git operations';
  Result.TimeRange := 'Last ' + IntToStr(Length(Entries)) + ' operations';
  
  // Find the most common operation type
  if Length(Entries) > 0 then
  begin
    Result.OperationType := Entries[0].Operation;
    Result.BeforeState := 'Previous state: ' + Entries[High(Entries)].ShortHash;
    Result.AfterState := 'Current state: ' + Entries[0].ShortHash;
  end;
  
  // Build a summary of changes
  SetLength(Result.Changes, 0);
  for i := 0 to Math.Min(4, High(Entries)) do
  begin
    SetLength(Result.Changes, Length(Result.Changes) + 1);
    Result.Changes[High(Result.Changes)] := Entries[i].Operation + ': ' + Entries[i].Subject;
  end;
  
  Result.Impact := 'Repository state has changed through ' + IntToStr(Length(Entries)) + ' operations';
  Result.Explanation := 'The repository has undergone several operations recently. The most recent operation was a ' + 
                       Result.OperationType + '. This analysis shows the sequence of changes that have occurred.';
  
  // TODO: Implement actual AI analysis using LLM
  try
    Config := TGitPalConfig.Create;
    try
      Registry := TProviderRegistry.Create;
      try
        // Determine which provider to use
        if ProviderOverride <> '' then
          ProviderName := ProviderOverride
        else
          ProviderName := Config.DefaultProvider;
        
        // Get the provider
        Provider := Registry.CreateProviderFromConfig(Config, ProviderName);
        if not Assigned(Provider) then
        begin
          DebugLog('[GenerateOperationAnalysis] Provider not found: ' + ProviderName);
          Exit; // Return simple analysis
        end;
        
        // Build reflog summary for AI
        ReflogSummary := '';
        for i := 0 to High(Entries) do
        begin
          ReflogSummary := ReflogSummary + Format('%s: %s (%s)%s', [
            Entries[i].ShortHash,
            Entries[i].Subject,
            Entries[i].Operation,
            #10
          ]);
        end;
        
        // Build system prompt
        SystemPrompt := 'You are a git expert analyzing recent repository operations. ' +
                       'Provide a clear, concise explanation of what happened during recent git operations. ' +
                       'Focus on the practical impact and what changed in the repository state.';
        
        // Build user prompt
        UserPrompt := 'Analyze these recent git reflog entries and explain what happened:' + #10 + #10 +
                     ReflogSummary + #10 +
                     'Please provide:' + #10 +
                     '1. The main operation type that occurred' + #10 +
                     '2. What the repository state was before and after' + #10 +
                     '3. The key changes that were made' + #10 +
                     '4. The overall impact and why these changes occurred' + #10 +
                     '5. A clear explanation of what happened';
        
        // Prepare messages
        SetLength(Messages, 2);
        Messages[0].Role := models.lmrSystem;
        Messages[0].Content := SystemPrompt;
        Messages[1].Role := models.lmrUser;
        Messages[1].Content := UserPrompt;
        
        // Call the AI
        Response := Provider.ChatCompletion(
          Provider.GetDefaultModel,
          Messages,
          0.3,    // Lower temperature for more consistent results
          2000    // Max tokens
        );
        
        if Length(Response.Choices) > 0 then
        begin
          // Parse AI response and update result
          Result.Explanation := Response.Choices[0].Message.Content;
          DebugLog('[GenerateOperationAnalysis] AI analysis completed successfully');
        end
        else
        begin
          DebugLog('[GenerateOperationAnalysis] AI analysis failed: No choices returned');
        end;
        
      finally
        Registry.Free;
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      DebugLog('[GenerateOperationAnalysis] Exception during AI analysis: ' + E.ClassName + ': ' + E.Message);
      // Keep the simple analysis we already built
    end;
  end;
end;

procedure RunExplainCommand(const ProviderOverride: string);
var
  Prog: TBobaUIProgram;
  Model: TExplainModel;
begin
  // Check if we're in a git repository
  if not git.TGitRepository.IsRepository('.') then
  begin
    writeln();
    writeln(bobastyle.ColorText(AnsiString('✗ Error: Not in a git repository'), bobastyle.cBrightRed));
    writeln('  Please run this command from within a git repository.');
    Exit;
  end;
  
  // Create model
  Model := TExplainModel.Create(ProviderOverride);
  
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  try
    Prog.Run;
  finally
    Prog.Free;
  end;
end;

end.