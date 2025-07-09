unit command_undo;

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
  SyncObjs;

type
  // Forward declarations
  TAsyncUndoOperation = class;
  TAsyncUndoOperationThread = class;

  // Progress stages for undo operation
  TUndoProgressStage = (
    psInitializing,          // Starting up
    psLoadingReflog,         // Reading git reflog
    psAnalyzingEntries,      // Parsing reflog entries
    psProcessingBatch,       // Processing current batch
    psGeneratingDescriptions,// AI generating explanations
    psAwaitingSelection,     // User selecting operation to undo
    psValidatingRepo,        // Checking repository state
    psCreatingBackup,        // Creating backup branch
    psAnalyzingImpact,       // Analyzing what will change
    psAwaitingConfirmation,  // User confirming recovery
    psExecutingRecovery,     // Performing the undo operation
    psCompleted,             // Success
    psError                  // Error occurred
  );

  // Async operation state enumeration
  TAsyncUndoOperationState = (
    osIdle,               // No operation running
    osAnalyzing,          // Analyzing reflog
    osGenerating,         // Generating descriptions
    osCancelling          // Cancelling current operation
  );

  // Recovery strategy enumeration
  TRecoveryStrategy = (
    rsReset,        // Use git reset --hard
    rsRevert,       // Use git revert
    rsCherryPick,   // Cherry-pick specific commits
    rsBranchRestore // Restore deleted branch
  );

  // Recovery plan data structure
  TRecoveryPlan = record
    Strategy: TRecoveryStrategy;
    TargetHash: string;
    BackupBranchName: string;
    ImpactDescription: string;
    AffectedFiles: array of string;
    CommitsToRemove: array of string;
    CommitsToRestore: array of string;
    RiskLevel: string; // 'low', 'medium', 'high'
    RequiresConfirmation: Boolean;
  end;

  // Async operation base class
  TAsyncUndoOperation = class
  private
    FThread: TAsyncUndoOperationThread;
    FOperationId: string;
    FIsRunning: Boolean;
    FCS: TCriticalSection;
  public
    constructor Create(const AOperationId: string);
    destructor Destroy; override;
    procedure Start; virtual; abstract;
    procedure Cancel;
    function IsRunning: Boolean;
    property OperationId: string read FOperationId;
  end;

  // Thread for async operations
  TAsyncUndoOperationThread = class(TThread)
  private
    FOperation: TAsyncUndoOperation;
  public
    constructor Create(AOperation: TAsyncUndoOperation);
    procedure Execute; override;
  end;

  // Progress update message
  TAsyncUndoProgressUpdateMsg = class(bobaui.TMsg)
  private
    FOperationId: string;
    FStage: TUndoProgressStage;
    FMessage: string;
  public
    constructor Create(const AOperationId: string; AStage: TUndoProgressStage; const AMessage: string);
    property OperationId: string read FOperationId;
    property Stage: TUndoProgressStage read FStage;
    property ProgressMessage: string read FMessage;
  end;

  // Result message for async operations
  TAsyncUndoResultMsg = class(bobaui.TMsg)
  private
    FOperationId: string;
    FSuccess: Boolean;
    FErrorMessage: string;
  public
    constructor Create(const AOperationId: string; ASuccess: Boolean; const AErrorMessage: string = '');
    property OperationId: string read FOperationId;
    property Success: Boolean read FSuccess;
    property ErrorMessage: string read FErrorMessage;
  end;

  // Reflog entry data structure
  TReflogEntry = record
    Hash: string;
    RefName: string;
    Operation: string;
    Message: string;
    Timestamp: string;
    IsUndoable: Boolean;
    AIDescription: string;  // AI-generated description
    UndoImpact: string;     // What undoing would do
    RiskWarning: string;    // Any risks or considerations
  end;

  // Batch of reflog entries for processing
  TReflogBatch = class
  private
    FEntries: array of TReflogEntry;
    FBatchNumber: Integer;
  public
    constructor Create(ABatchNumber: Integer);
    destructor Destroy; override;
    procedure AddEntry(const Entry: TReflogEntry);
    function GetEntry(Index: Integer): TReflogEntry;
    procedure SetEntry(Index: Integer; const Entry: TReflogEntry);
    function Count: Integer;
    property BatchNumber: Integer read FBatchNumber;
  end;

  // Concrete async operation for reflog analysis
  TAsyncReflogAnalysis = class(TAsyncUndoOperation)
  private
    FCustomPrompt: string;
    FCurrentBatchIndex: Integer;
  public
    FBatches: array of TReflogBatch; // Public for access by TUndoModel
    constructor Create(const AOperationId: string; const ACustomPrompt: string);
    destructor Destroy; override;
    procedure Start; override;
    procedure LoadReflogEntries;
  private
    procedure ParseReflogLine(const Line: string; out Entry: TReflogEntry);
    function IsOperationUndoable(const Operation: string): Boolean;
    procedure CreateBatches(const Entries: array of TReflogEntry);
    procedure ProcessBatch(BatchIndex: Integer);
    function BuildBatchPrompt(const Batch: TReflogBatch): string;
    procedure ProcessAIResponse(const Batch: TReflogBatch; const AIResponse: string);
  end;

  // Main undo model
  TUndoModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FMessage: string;
    FSpinner: bobacomponents.TSpinner;
    FCustomPrompt: string;
    
    // Async operation fields
    FAsyncState: TAsyncUndoOperationState;
    FOperationId: string;
    FCurrentStage: TUndoProgressStage;
    FCurrentOperation: TAsyncUndoOperation;
    
    // Progress tracking
    FLoadingReflogCompleted: Boolean;
    FAnalyzingEntriesCompleted: Boolean;
    FProcessingBatchCompleted: Boolean;
    FGeneratingDescriptionsCompleted: Boolean;
    
    // Interactive selection
    FOperationList: bobacomponents.TList;
    FUndoableEntries: array of TReflogEntry;
    FSelectedIndex: Integer;
    FShowingSelection: Boolean;
    
    // Recovery execution
    FRecoveryPlan: TRecoveryPlan;
    FShowingConfirmation: Boolean;
    FValidatingRepo: Boolean;
    FCreatingBackup: Boolean;
    FAnalyzingImpact: Boolean;
    FExecutingRecovery: Boolean;
    
    function GenerateOperationId: string;
    procedure StartUndoAnalysis;
    procedure HandleAsyncProgressUpdate(const Msg: TAsyncUndoProgressUpdateMsg);
    procedure HandleAsyncResult(const Msg: TAsyncUndoResultMsg);
    procedure InitializeOperationList;
    procedure PopulateOperationList;
    procedure HandleListSelection(const Msg: bobacomponents.TListSelectionMsg);
    
    // Recovery operations
    procedure StartRecoveryProcess(const Entry: TReflogEntry);
    function CreateRecoveryPlan(const Entry: TReflogEntry): TRecoveryPlan;
    function ValidateRepositoryState: Boolean;
    function CreateBackupBranch(const BranchName: string): Boolean;
    function AnalyzeRecoveryImpact(const Plan: TRecoveryPlan): Boolean;
    function ExecuteRecovery(const Plan: TRecoveryPlan): Boolean;
    function GenerateBackupBranchName: string;
    function GetStrategyDescription(Strategy: TRecoveryStrategy): string;
    function GetRiskLevelDisplay(const RiskLevel: string): string;
  public
    constructor Create(const ACustomPrompt: string = '');
    destructor Destroy; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
    function View: string; override;
  end;

// Public functions
procedure RunUndoCommand(const CustomPrompt: string = '');

implementation

uses
  DateUtils, Math;

var
  GProgram: bobaui.TBobaUIProgram = nil;

// Helper function to send async progress updates
procedure SendAsyncUndoProgressUpdate(const AOperationId: string; AStage: TUndoProgressStage; const AMessage: string);
var
  ProgressMsg: TAsyncUndoProgressUpdateMsg;
begin
  if Assigned(GProgram) and (AOperationId <> '') then
  begin
    ProgressMsg := TAsyncUndoProgressUpdateMsg.Create(AOperationId, AStage, AMessage);
    GProgram.Send(ProgressMsg);
  end;
end;

{ TAsyncUndoOperation }

constructor TAsyncUndoOperation.Create(const AOperationId: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FIsRunning := False;
  FCS := TCriticalSection.Create;
end;

destructor TAsyncUndoOperation.Destroy;
begin
  Cancel;
  FCS.Free;
  inherited Destroy;
end;

procedure TAsyncUndoOperation.Cancel;
begin
  FCS.Enter;
  try
    if FIsRunning and Assigned(FThread) then
    begin
      FThread.Terminate;
      FThread.WaitFor;
      FreeAndNil(FThread);
    end;
    FIsRunning := False;
  finally
    FCS.Leave;
  end;
end;

function TAsyncUndoOperation.IsRunning: Boolean;
begin
  FCS.Enter;
  try
    Result := FIsRunning;
  finally
    FCS.Leave;
  end;
end;

{ TAsyncUndoOperationThread }

constructor TAsyncUndoOperationThread.Create(AOperation: TAsyncUndoOperation);
begin
  inherited Create(False);
  FOperation := AOperation;
  FreeOnTerminate := False;
end;

procedure TAsyncUndoOperationThread.Execute;
begin
  try
    // Execute the actual work on the thread
    if FOperation is TAsyncReflogAnalysis then
      TAsyncReflogAnalysis(FOperation).LoadReflogEntries;
  except
    on E: Exception do
    begin
      // Send error message
      if Assigned(GProgram) then
      begin
        GProgram.Send(TAsyncUndoResultMsg.Create(FOperation.OperationId, False, E.Message));
      end;
    end;
  end;
end;

{ TAsyncUndoProgressUpdateMsg }

constructor TAsyncUndoProgressUpdateMsg.Create(const AOperationId: string; AStage: TUndoProgressStage; const AMessage: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FStage := AStage;
  FMessage := AMessage;
end;

{ TAsyncUndoResultMsg }

constructor TAsyncUndoResultMsg.Create(const AOperationId: string; ASuccess: Boolean; const AErrorMessage: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FSuccess := ASuccess;
  FErrorMessage := AErrorMessage;
end;

{ TReflogBatch }

constructor TReflogBatch.Create(ABatchNumber: Integer);
begin
  inherited Create;
  FBatchNumber := ABatchNumber;
  SetLength(FEntries, 0);
end;

destructor TReflogBatch.Destroy;
begin
  SetLength(FEntries, 0);
  inherited Destroy;
end;

procedure TReflogBatch.AddEntry(const Entry: TReflogEntry);
begin
  SetLength(FEntries, Length(FEntries) + 1);
  FEntries[High(FEntries)] := Entry;
end;

function TReflogBatch.GetEntry(Index: Integer): TReflogEntry;
begin
  if (Index >= 0) and (Index < Length(FEntries)) then
    Result := FEntries[Index]
  else
    raise Exception.Create('Batch entry index out of range');
end;

procedure TReflogBatch.SetEntry(Index: Integer; const Entry: TReflogEntry);
begin
  if (Index >= 0) and (Index < Length(FEntries)) then
    FEntries[Index] := Entry
  else
    raise Exception.Create('Batch entry index out of range');
end;

function TReflogBatch.Count: Integer;
begin
  Result := Length(FEntries);
end;

{ TAsyncReflogAnalysis }

constructor TAsyncReflogAnalysis.Create(const AOperationId: string; const ACustomPrompt: string);
begin
  inherited Create(AOperationId);
  FCustomPrompt := ACustomPrompt;
  SetLength(FBatches, 0);
  FCurrentBatchIndex := 0;
end;

destructor TAsyncReflogAnalysis.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FBatches) do
    FBatches[I].Free;
  SetLength(FBatches, 0);
  inherited Destroy;
end;

procedure TAsyncReflogAnalysis.Start;
begin
  FCS.Enter;
  try
    if not FIsRunning then
    begin
      FIsRunning := True;
      FThread := TAsyncUndoOperationThread.Create(Self);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TAsyncReflogAnalysis.LoadReflogEntries;
var
  GitResult: TGitResult;
  Lines: TStringList;
  I: Integer;
  Entry: TReflogEntry;
  Entries: array of TReflogEntry;
begin
  SendAsyncUndoProgressUpdate(FOperationId, psLoadingReflog, 'Loading git reflog entries...');
  
  // Get reflog with detailed format including commit subject
  DebugLog('UNDO: Calling GetReflog with format: --format=%H|%gd|%gs|%cr|%s');
  
  // Test direct git execution to compare
  DebugLog('UNDO: Testing direct git command...');
  GitResult := TGitRepository.Execute(['reflog', '--format=%H|%gd|%gs|%cr|%s']);
  DebugLog('UNDO: Direct git result - Success: ' + BoolToStr(GitResult.Success, True));
  DebugLog('UNDO: Direct git output: ' + Copy(GitResult.Output, 1, 200));
  
  // Now try the GetReflog method
  GitResult := TGitRepository.GetReflog(['--format=%H|%gd|%gs|%cr|%s']);
  DebugLog('UNDO: GetReflog result - Success: ' + BoolToStr(GitResult.Success, True));
  DebugLog('UNDO: GetReflog full output: ' + GitResult.Output);
  
  if not GitResult.Success then
    raise Exception.Create('Failed to load reflog: ' + GitResult.ErrorMessage);
  
  // Debug log the raw reflog output
  DebugLog('UNDO: Raw reflog output (first 200 chars): ' + Copy(GitResult.Output, 1, 200));
    
  SendAsyncUndoProgressUpdate(FOperationId, psAnalyzingEntries, 'Parsing reflog entries...');
  
  Lines := TStringList.Create;
  try
    // IMPORTANT: Don't use Lines.Text as it corrupts pipe characters!
    // Manually split by line breaks to preserve pipes
    Lines.StrictDelimiter := True;
    Lines.Delimiter := #10; // Use LF as delimiter
    Lines.DelimitedText := StringReplace(GitResult.Output, #13#10, #10, [rfReplaceAll]); // Convert CRLF to LF
    
    DebugLog('UNDO: Lines.Count after manual splitting: ' + IntToStr(Lines.Count));
    if Lines.Count > 0 then
      DebugLog('UNDO: First line after manual split: ' + Lines[0]);
    
    SetLength(Entries, 0);
    
    // Parse each reflog line (limit to 50 most recent by default)
    for I := 0 to Min(Lines.Count - 1, 49) do
    begin
      if Trim(Lines[I]) <> '' then
      begin
        ParseReflogLine(Lines[I], Entry);
        
        // Debug log each parsed entry
        DebugLog('UNDO: Parsed entry - Hash: ' + Copy(Entry.Hash, 1, 8) + 
                ', Op: ' + Entry.Operation + ', Time: ' + Entry.Timestamp + 
                ', Undoable: ' + BoolToStr(Entry.IsUndoable, True));
        
        if Entry.IsUndoable then
        begin
          SetLength(Entries, Length(Entries) + 1);
          Entries[High(Entries)] := Entry;
        end;
      end;
    end;
    
  finally
    Lines.Free;
  end;
  
  SendAsyncUndoProgressUpdate(FOperationId, psProcessingBatch, 'Creating processing batches...');
  
  // Create batches for processing
  CreateBatches(Entries);
  
  // Process each batch
  for I := 0 to High(FBatches) do
  begin
    ProcessBatch(I);
    SendAsyncUndoProgressUpdate(FOperationId, psProcessingBatch, 
      'Processed batch ' + IntToStr(I + 1) + ' of ' + IntToStr(Length(FBatches)));
  end;
  
  // Signal completion
  if Assigned(GProgram) then
    GProgram.Send(TAsyncUndoResultMsg.Create(FOperationId, True));
end;

procedure TAsyncReflogAnalysis.ParseReflogLine(const Line: string; out Entry: TReflogEntry);
var
  Parts: TStringList;
  CommitMessage: string;
begin
  DebugLog('UNDO: Parsing line: ' + Copy(Line, 1, 100));
  
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '|';
    Parts.StrictDelimiter := True; // Important: don't treat spaces as delimiters
    Parts.DelimitedText := Line;
    
    DebugLog('UNDO: Split into ' + IntToStr(Parts.Count) + ' parts');
    if Parts.Count > 0 then DebugLog('UNDO: Part 0: ' + Parts[0]);
    if Parts.Count > 1 then DebugLog('UNDO: Part 1: ' + Parts[1]);
    if Parts.Count > 2 then DebugLog('UNDO: Part 2: ' + Copy(Parts[2], 1, 50));
    
    if Parts.Count >= 4 then
    begin
      Entry.Hash := Parts[0];
      Entry.RefName := Parts[1];
      Entry.Operation := Parts[2];
      Entry.Timestamp := Parts[3];
      
      // Get commit message if available
      if Parts.Count >= 5 then
        CommitMessage := Parts[4]
      else
        CommitMessage := '';
      
      // Build a better message
      if CommitMessage <> '' then
        Entry.Message := Entry.Operation + ' - "' + CommitMessage + '"'
      else
        Entry.Message := Entry.Operation;
      
      Entry.IsUndoable := IsOperationUndoable(Entry.Operation);
      
      DebugLog('UNDO: Parsed - Hash: ' + Copy(Entry.Hash, 1, 8) + ', Op: ' + Entry.Operation + ', Undoable: ' + BoolToStr(Entry.IsUndoable, True));
    end
    else
    begin
      DebugLog('UNDO: Not enough parts in line, skipping');
      Entry.Hash := '';
      Entry.RefName := '';
      Entry.Operation := '';
      Entry.Message := '';
      Entry.Timestamp := '';
      Entry.IsUndoable := False;
    end;
    
    // Initialize AI fields
    Entry.AIDescription := '';
    Entry.UndoImpact := '';
    Entry.RiskWarning := '';
  finally
    Parts.Free;
  end;
end;

function TAsyncReflogAnalysis.IsOperationUndoable(const Operation: string): Boolean;
var
  OpLower: string;
begin
  OpLower := LowerCase(Operation);
  
  // Focus on destructive operations that users often want to undo
  Result := (Pos('reset', OpLower) > 0) or                    // Hard resets lose commits
            (Pos('rebase', OpLower) > 0) or                   // Rebases can be confusing
            (Pos('merge', OpLower) > 0) or                    // Merges can introduce conflicts
            (Pos('cherry-pick', OpLower) > 0) or              // Cherry-picks can duplicate commits
            (Pos('pull', OpLower) > 0) or                     // Pulls can introduce unwanted changes
            ((Pos('checkout', OpLower) > 0) and 
             ((Pos('moving from', OpLower) > 0) or            // Branch switches that might lose work
              (Pos('force', OpLower) > 0))) or                // Forced checkouts
            ((Pos('branch', OpLower) > 0) and 
             (Pos('-d', OpLower) > 0)) or                     // Branch deletions
            (Pos('commit --amend', OpLower) > 0);             // Amended commits
end;

procedure TAsyncReflogAnalysis.CreateBatches(const Entries: array of TReflogEntry);
const
  BATCH_SIZE = 10; // Process 10 entries per batch
var
  I, BatchIndex: Integer;
  CurrentBatch: TReflogBatch;
begin
  BatchIndex := 0;
  CurrentBatch := nil;
  
  for I := 0 to High(Entries) do
  begin
    // Create new batch if needed
    if (CurrentBatch = nil) or (CurrentBatch.Count >= BATCH_SIZE) then
    begin
      if CurrentBatch <> nil then
      begin
        // Add completed batch to array
        SetLength(FBatches, Length(FBatches) + 1);
        FBatches[High(FBatches)] := CurrentBatch;
      end;
      
      Inc(BatchIndex);
      CurrentBatch := TReflogBatch.Create(BatchIndex);
    end;
    
    CurrentBatch.AddEntry(Entries[I]);
  end;
  
  // Add final batch if it has entries
  if (CurrentBatch <> nil) and (CurrentBatch.Count > 0) then
  begin
    SetLength(FBatches, Length(FBatches) + 1);
    FBatches[High(FBatches)] := CurrentBatch;
  end;
end;

procedure TAsyncReflogAnalysis.ProcessBatch(BatchIndex: Integer);
var
  Batch: TReflogBatch;
  I: Integer;
  Entry: TReflogEntry;
  AIPrompt: string;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  Provider: ILLMProvider;
  SystemPrompt: string;
begin
  if (BatchIndex >= 0) and (BatchIndex < Length(FBatches)) then
  begin
    Batch := FBatches[BatchIndex];
    
    SendAsyncUndoProgressUpdate(FOperationId, psGeneratingDescriptions, 
      'Generating descriptions for batch ' + IntToStr(BatchIndex + 1) + '...');
    
    try
      // Load configuration and provider
      Config := TGitPalConfig.Create;
      Registry := TProviderRegistry.Create;
      try
        DebugLog('UNDO: Loading config and creating provider...');
        if not Config.LoadConfig then
        begin
          DebugLog('UNDO: Failed to load config file');
          raise Exception.Create('Configuration file not found or invalid');
        end;
        DebugLog('UNDO: Config default provider: ' + string(Config.DefaultProvider));
        
        try
          Provider := Registry.CreateDefaultProvider(Config);
          DebugLog('UNDO: CreateDefaultProvider call completed');
        except
          on E: Exception do
          begin
            DebugLog('UNDO: CreateDefaultProvider failed: ' + E.Message);
            DebugLog('UNDO: CreateDefaultProvider exception class: ' + E.ClassName);
            raise; // Re-raise the original exception
          end;
        end;
        
        if Provider = nil then
        begin
          DebugLog('UNDO: Provider is nil after CreateDefaultProvider');
          raise Exception.Create('Provider not configured');
        end;
        
        DebugLog('UNDO: Provider created successfully');
        
        // Build AI prompt
        AIPrompt := BuildBatchPrompt(Batch);
        SystemPrompt := 'You are an expert git assistant helping users recover from git mistakes. ' +
                       'Analyze reflog entries and provide clear, user-friendly explanations. ' +
                       'Focus on what the user did (not git internals) and what they would recover by undoing it. ' +
                       'Be specific about commits, branches, and files affected when this information is available.';
        
        // Build messages
        SetLength(Messages, 2);
        Messages[0].Role := models.lmrSystem;
        Messages[0].Content := SystemPrompt;
        Messages[1].Role := models.lmrUser;
        Messages[1].Content := AIPrompt;
        
        // Send to AI
        SendAsyncUndoProgressUpdate(FOperationId, psGeneratingDescriptions, 
          AnsiString('   Sending batch to AI for analysis...'));
        
        // Debug log the prompt being sent
        DebugLog('UNDO: Calling AI provider for batch ' + IntToStr(BatchIndex + 1));
        DebugLog('UNDO: AI Prompt: ' + AIPrompt);
          
        Response := Provider.ChatCompletion(Provider.GetDefaultModel, Messages, 0.3, 150);
        
        // Debug log the response
        if Length(Response.Choices) > 0 then
        begin
          DebugLog('UNDO: Got AI response, length: ' + IntToStr(Length(Response.Choices[0].Message.Content)));
          DebugLog('UNDO: AI Response: ' + Response.Choices[0].Message.Content);
        end;
        
        if Length(Response.Choices) > 0 then
        begin
          // Process AI response and update batch entries
          ProcessAIResponse(Batch, Response.Choices[0].Message.Content);
          SendAsyncUndoProgressUpdate(FOperationId, psGeneratingDescriptions, 
            AnsiString('   ✓ Generated descriptions for batch ') + AnsiString(IntToStr(BatchIndex + 1)));
        end
        else
        begin
          // No response from AI
          DebugLog('UNDO: No AI response received');
        end;
      finally
        Registry.Free;
        Config.Free;
      end;
    except
      on E: Exception do
      begin
        // Log the full error for debugging
        DebugLog('UNDO: AI processing error: ' + E.Message);
        DebugLog('UNDO: Error class: ' + E.ClassName);
        
        // Check if this is an authentication error
        if (Pos('401', E.Message) > 0) or 
           (Pos('unauthorized', LowerCase(E.Message)) > 0) or
           (Pos('authentication', LowerCase(E.Message)) > 0) or
           (Pos('oauth', LowerCase(E.Message)) > 0) or
           (Pos('token', LowerCase(E.Message)) > 0) or
           (Pos('provider configured', LowerCase(E.Message)) > 0) then
        begin
          // This is likely an OAuth/authentication error - propagate it
          raise Exception.Create('Authentication failed: ' + E.Message + 
            '. Your OAuth token may have expired. Please run "gitpal setup" to re-authenticate.');
        end
        else
        begin
          // Other errors - fail completely since gitpal requires AI
          DebugLog('UNDO: AI processing failed: ' + E.Message);
          raise Exception.Create('AI processing failed: ' + E.Message + 
            '. gitpal undo requires AI assistance. Please check your configuration.');
        end;
      end;
    end;
  end;
end;

function TAsyncReflogAnalysis.BuildBatchPrompt(const Batch: TReflogBatch): string;
var
  I: Integer;
  Entry: TReflogEntry;
  Prompt: TStringList;
begin
  Prompt := TStringList.Create;
  try
    Prompt.Add('Analyze these git reflog entries and provide clear explanations:');
    Prompt.Add('');
    
    if FCustomPrompt <> '' then
    begin
      Prompt.Add('User context: ' + FCustomPrompt);
      Prompt.Add('');
    end;
    
    Prompt.Add('Reflog entries to analyze:');
    for I := 0 to Batch.Count - 1 do
    begin
      Entry := Batch.GetEntry(I);
      Prompt.Add(Format('%d. Hash: %s, Operation: %s, Time: %s, Message: %s', 
        [I + 1, Copy(Entry.Hash, 1, 8), Entry.Operation, Entry.Timestamp, Entry.Message]));
    end;
    
    Prompt.Add('');
    Prompt.Add('For each entry, provide a response in EXACTLY this format:');
    Prompt.Add('');
    Prompt.Add('Entry 1:');
    Prompt.Add('Description: [One-line description of what happened]');
    Prompt.Add('Undo effect: [What undoing this would do]');
    Prompt.Add('Risk: [low/medium/high]');
    Prompt.Add('');
    Prompt.Add('Entry 2:');
    Prompt.Add('...(continue for all entries)');
    Prompt.Add('');
    Prompt.Add('Keep descriptions concise and focus on what the user would care about.');
    Prompt.Add('For descriptions, use this style:');
    Prompt.Add('- Reset: "Reset to [commit/branch] (removed X commits including [notable changes])"');
    Prompt.Add('- Rebase: "Rebased X commits onto [branch] (rewrote history)"');
    Prompt.Add('- Merge: "Merged [branch] (brought in X commits)"');
    Prompt.Add('- Deletion: "Deleted branch [name] (contained X commits)"');
    Prompt.Add('Make the descriptions actionable and clear about consequences.');
    
    Result := Prompt.Text;
  finally
    Prompt.Free;
  end;
end;

procedure TAsyncReflogAnalysis.ProcessAIResponse(const Batch: TReflogBatch; const AIResponse: string);
var
  Lines: TStringList;
  I, EntryIndex: Integer;
  Entry: TReflogEntry;
  CurrentLine: string;
  InEntry: Boolean;
  Description, UndoEffect, Risk: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AIResponse;
    
    // Debug log the full AI response being processed
    DebugLog('UNDO: Processing AI response: ' + Copy(AIResponse, 1, 500));
    
    EntryIndex := -1;
    InEntry := False;
    Description := '';
    UndoEffect := '';
    Risk := 'medium';
    
    // Parse AI response line by line
    for I := 0 to Lines.Count - 1 do
    begin
      CurrentLine := Trim(Lines[I]);
      
      // Check for entry marker
      if (Pos('Entry ', CurrentLine) = 1) and (Pos(':', CurrentLine) > 0) then
      begin
        // Save previous entry if exists
        if (EntryIndex >= 0) and (EntryIndex < Batch.Count) and InEntry then
        begin
          Entry := Batch.GetEntry(EntryIndex);
          Entry.AIDescription := Description;
          Entry.UndoImpact := UndoEffect;
          Entry.RiskWarning := Risk;
          Batch.SetEntry(EntryIndex, Entry);
        end;
        
        // Start new entry
        InEntry := True;
        EntryIndex := EntryIndex + 1;
        Description := '';
        UndoEffect := '';
        Risk := 'medium';
      end
      else if InEntry and (EntryIndex < Batch.Count) then
      begin
        // Parse entry fields
        if Pos('Description:', CurrentLine) = 1 then
          Description := Trim(Copy(CurrentLine, Length('Description:') + 1, MaxInt))
        else if Pos('Undo effect:', CurrentLine) = 1 then
          UndoEffect := Trim(Copy(CurrentLine, Length('Undo effect:') + 1, MaxInt))
        else if Pos('Risk:', CurrentLine) = 1 then
          Risk := Trim(Copy(CurrentLine, Length('Risk:') + 1, MaxInt));
      end;
    end;
    
    // Save last entry
    if (EntryIndex >= 0) and (EntryIndex < Batch.Count) and InEntry then
    begin
      Entry := Batch.GetEntry(EntryIndex);
      Entry.AIDescription := Description;
      Entry.UndoImpact := UndoEffect;
      Entry.RiskWarning := Risk;
      Batch.SetEntry(EntryIndex, Entry);
    end;
    
    // Log successful processing
    for I := 0 to Batch.Count - 1 do
    begin
      Entry := Batch.GetEntry(I);
      DebugLog('UNDO: Entry ' + IntToStr(I) + ' - AI Description: "' + Entry.AIDescription + 
              '", Undo: "' + Entry.UndoImpact + '", Risk: "' + Entry.RiskWarning + '"');
      SendAsyncUndoProgressUpdate(FOperationId, psGeneratingDescriptions, 
        AnsiString('   ✓ Processed entry: ') + AnsiString(Entry.AIDescription));
    end;
  finally
    Lines.Free;
  end;
end;

{ TUndoModel }

constructor TUndoModel.Create(const ACustomPrompt: string);
begin
  inherited Create;
  FCustomPrompt := ACustomPrompt;
  FMessage := AnsiString('Initializing undo command...');
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
  
  // Initialize terminal dimensions (will be set by WindowSizeMsg)
  FTerminalWidth := 0;
  FTerminalHeight := 0;
  
  // Initialize async operation fields
  FAsyncState := osIdle;
  FOperationId := AnsiString('');
  FCurrentStage := psInitializing;
  FCurrentOperation := nil;
  
  // Initialize progress tracking
  FLoadingReflogCompleted := False;
  FAnalyzingEntriesCompleted := False;
  FProcessingBatchCompleted := False;
  FGeneratingDescriptionsCompleted := False;
  
  // Initialize interactive selection
  SetLength(FUndoableEntries, 0);
  FSelectedIndex := -1;
  FShowingSelection := False;
  InitializeOperationList;
  
  // Initialize recovery execution
  FShowingConfirmation := False;
  FValidatingRepo := False;
  FCreatingBackup := False;
  FAnalyzingImpact := False;
  FExecutingRecovery := False;
  
  // Start the analysis process
  StartUndoAnalysis;
end;

destructor TUndoModel.Destroy;
begin
  if Assigned(FCurrentOperation) then
  begin
    FCurrentOperation.Cancel;
    FCurrentOperation.Free;
  end;
  FSpinner.Free;
  FOperationList.Free;
  SetLength(FUndoableEntries, 0);
  inherited Destroy;
end;

function TUndoModel.GenerateOperationId: string;
begin
  Result := 'undo_op_' + IntToStr(GetTickCount64);
end;

procedure TUndoModel.StartUndoAnalysis;
begin
  if FAsyncState = osIdle then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osAnalyzing;
    FCurrentStage := psLoadingReflog;
    FMessage := AnsiString('Loading git reflog...');
    
    // Start the async reflog analysis
    FCurrentOperation := TAsyncReflogAnalysis.Create(FOperationId, FCustomPrompt);
    FCurrentOperation.Start;
  end;
end;

procedure TUndoModel.HandleAsyncProgressUpdate(const Msg: TAsyncUndoProgressUpdateMsg);
begin
  if Msg.OperationId = FOperationId then
  begin
    FCurrentStage := Msg.Stage;
    FMessage := Msg.ProgressMessage;
    
    // Update completion flags based on stage
    case Msg.Stage of
      psLoadingReflog: FLoadingReflogCompleted := False;
      psAnalyzingEntries: begin
        FLoadingReflogCompleted := True;
        FAnalyzingEntriesCompleted := False;
      end;
      psProcessingBatch: begin
        FAnalyzingEntriesCompleted := True;
        FProcessingBatchCompleted := False;
      end;
      psGeneratingDescriptions: begin
        FProcessingBatchCompleted := True;
        FGeneratingDescriptionsCompleted := False;
      end;
      psAwaitingSelection: begin
        FGeneratingDescriptionsCompleted := True;
        FAsyncState := osIdle;
      end;
      psError: FAsyncState := osIdle;
    end;
  end;
end;

procedure TUndoModel.HandleAsyncResult(const Msg: TAsyncUndoResultMsg);
begin
  if Msg.OperationId = FOperationId then
  begin
    if Msg.Success then
    begin
      FCurrentStage := psAwaitingSelection;
      FMessage := AnsiString('Select an operation to undo:');
      FShowingSelection := True;
      // Collect entries from all batches
      if Assigned(FCurrentOperation) and (FCurrentOperation is TAsyncReflogAnalysis) then
        PopulateOperationList;
    end
    else
    begin
      FCurrentStage := psError;
      FMessage := AnsiString('Error: ') + Msg.ErrorMessage;
    end;
    FAsyncState := osIdle;
  end;
end;

procedure TUndoModel.InitializeOperationList;
begin
  FOperationList := bobacomponents.TList.Create;
  FOperationList.Width := 80;
  FOperationList.Height := 10;
  FOperationList.ShowBorder := True;
  FOperationList.SelectedColor := bobastyle.cBrightBlue;
end;

procedure TUndoModel.PopulateOperationList;
var
  Analysis: TAsyncReflogAnalysis;
  I, J: Integer;
  Batch: TReflogBatch;
  Entry: TReflogEntry;
  DisplayText: string;
begin
  if not (FCurrentOperation is TAsyncReflogAnalysis) then
    Exit;
    
  Analysis := TAsyncReflogAnalysis(FCurrentOperation);
  
  // Clear existing entries
  SetLength(FUndoableEntries, 0);
  FOperationList.ClearItems;
  
  // Collect all entries from all batches
  for I := 0 to High(Analysis.FBatches) do
  begin
    Batch := Analysis.FBatches[I];
    for J := 0 to Batch.Count - 1 do
    begin
      Entry := Batch.GetEntry(J);
      
      // Add to our array
      SetLength(FUndoableEntries, Length(FUndoableEntries) + 1);
      FUndoableEntries[High(FUndoableEntries)] := Entry;
      
      // Create display text
      if Entry.AIDescription <> '' then
      begin
        // Use AI description if available
        DisplayText := Format('%s (%s)', [
          Entry.AIDescription,
          Entry.Timestamp
        ]);
      end
      else
      begin
        // Fallback to basic format
        DisplayText := Format('%s (%s)', [
          Entry.Operation,
          Entry.Timestamp
        ]);
      end;
      
      // Add to list
      FOperationList.AddItem(DisplayText);
    end;
  end;
  
  if Length(FUndoableEntries) > 0 then
  begin
    FOperationList.SelectedIndex := 0;
    FSelectedIndex := 0;
  end;
end;

procedure TUndoModel.HandleListSelection(const Msg: bobacomponents.TListSelectionMsg);
begin
  FSelectedIndex := Msg.SelectedIndex;
  // Update message to show selected operation details
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FUndoableEntries)) then
  begin
    FMessage := AnsiString('Selected: ') + FUndoableEntries[FSelectedIndex].Operation;
  end;
end;

procedure TUndoModel.StartRecoveryProcess(const Entry: TReflogEntry);
begin
  FCurrentStage := psValidatingRepo;
  FMessage := AnsiString('Validating repository state...');
  FShowingSelection := False;
  FValidatingRepo := True;
  
  // Start recovery process
  if ValidateRepositoryState then
  begin
    FCurrentStage := psAnalyzingImpact;
    FMessage := AnsiString('Analyzing recovery impact...');
    FRecoveryPlan := CreateRecoveryPlan(Entry);
    
    if AnalyzeRecoveryImpact(FRecoveryPlan) then
    begin
      FCurrentStage := psAwaitingConfirmation;
      FMessage := AnsiString('Ready to execute recovery. Review the plan below.');
      FShowingConfirmation := True;
      FValidatingRepo := False;
    end
    else
    begin
      FCurrentStage := psError;
      FMessage := AnsiString('Failed to analyze recovery impact');
      FValidatingRepo := False;
    end;
  end
  else
  begin
    FCurrentStage := psError;
    FMessage := AnsiString('Repository validation failed. Please commit or stash changes first.');
    FValidatingRepo := False;
  end;
end;

function TUndoModel.CreateRecoveryPlan(const Entry: TReflogEntry): TRecoveryPlan;
var
  OpLower: string;
begin
  // Initialize plan
  Result.TargetHash := Entry.Hash;
  Result.BackupBranchName := GenerateBackupBranchName;
  Result.RequiresConfirmation := True;
  SetLength(Result.AffectedFiles, 0);
  SetLength(Result.CommitsToRemove, 0);
  SetLength(Result.CommitsToRestore, 0);
  
  OpLower := LowerCase(Entry.Operation);
  
  // Determine strategy based on operation type
  if (Pos('reset', OpLower) > 0) or (Pos('rebase', OpLower) > 0) then
  begin
    Result.Strategy := rsReset;
    Result.ImpactDescription := 'Reset HEAD to ' + Copy(Entry.Hash, 1, 8) + ' (before ' + Entry.Operation + ')';
    Result.RiskLevel := 'high';
  end
  else if Pos('merge', OpLower) > 0 then
  begin
    Result.Strategy := rsRevert;
    Result.ImpactDescription := 'Revert merge commit ' + Copy(Entry.Hash, 1, 8);
    Result.RiskLevel := 'medium';
  end
  else if Pos('cherry-pick', OpLower) > 0 then
  begin
    Result.Strategy := rsCherryPick;
    Result.ImpactDescription := 'Remove cherry-picked commit ' + Copy(Entry.Hash, 1, 8);
    Result.RiskLevel := 'low';
  end
  else if Pos('branch', OpLower) > 0 then
  begin
    Result.Strategy := rsBranchRestore;
    Result.ImpactDescription := 'Restore branch from reflog entry ' + Copy(Entry.Hash, 1, 8);
    Result.RiskLevel := 'low';
  end
  else
  begin
    // Default to reset for unknown operations
    Result.Strategy := rsReset;
    Result.ImpactDescription := 'Reset to state before ' + Entry.Operation;
    Result.RiskLevel := 'high';
  end;
end;

function TUndoModel.ValidateRepositoryState: Boolean;
var
  StatusResult: TGitResult;
begin
  Result := False;
  
  // Check if working tree is clean
  StatusResult := TGitRepository.Execute(['status', '--porcelain']);
  if StatusResult.Success then
  begin
    // If output is empty, working tree is clean
    Result := Trim(StatusResult.Output) = '';
  end;
end;

function TUndoModel.CreateBackupBranch(const BranchName: string): Boolean;
var
  CreateResult: TGitResult;
begin
  CreateResult := TGitRepository.CreateBranch(BranchName, 'HEAD');
  Result := CreateResult.Success;
  
  if not Result then
    FMessage := AnsiString('Failed to create backup branch: ') + CreateResult.ErrorMessage;
end;

function TUndoModel.AnalyzeRecoveryImpact(const Plan: TRecoveryPlan): Boolean;
var
  DiffResult: TGitResult;
  Lines: TStringList;
  I: Integer;
begin
  Result := False;
  
  try
    // Get diff to see what files will be affected
    case Plan.Strategy of
      rsReset:
        DiffResult := TGitRepository.GetDiff(['--name-only', Plan.TargetHash + '..HEAD']);
      rsRevert:
        DiffResult := TGitRepository.GetDiff(['--name-only', Plan.TargetHash + '^..' + Plan.TargetHash]);
      else
        DiffResult := TGitRepository.GetDiff(['--name-only', 'HEAD~1..HEAD']);
    end;
    
    if DiffResult.Success then
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := DiffResult.Output;
        
        // Store affected files
        SetLength(FRecoveryPlan.AffectedFiles, Lines.Count);
        for I := 0 to Lines.Count - 1 do
        begin
          if Trim(Lines[I]) <> '' then
            FRecoveryPlan.AffectedFiles[I] := Trim(Lines[I]);
        end;
        
        Result := True;
      finally
        Lines.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      FMessage := AnsiString('Impact analysis failed: ') + E.Message;
      Result := False;
    end;
  end;
end;

function TUndoModel.ExecuteRecovery(const Plan: TRecoveryPlan): Boolean;
var
  RecoveryResult: TGitResult;
begin
  Result := False;
  
  FCurrentStage := psCreatingBackup;
  FMessage := AnsiString('Creating backup branch...');
  
  // Create backup branch first
  if not CreateBackupBranch(Plan.BackupBranchName) then
    Exit;
    
  FCurrentStage := psExecutingRecovery;
  FMessage := AnsiString('Executing recovery operation...');
  
  try
    // Execute the appropriate recovery strategy
    case Plan.Strategy of
      rsReset:
        begin
          RecoveryResult := TGitRepository.Reset(Plan.TargetHash, '--hard');
          Result := RecoveryResult.Success;
        end;
      rsRevert:
        begin
          // Use git revert for merge commits
          RecoveryResult := TGitRepository.Execute(['revert', '-m', '1', Plan.TargetHash]);
          Result := RecoveryResult.Success;
        end;
      rsCherryPick:
        begin
          // Revert the cherry-pick
          RecoveryResult := TGitRepository.Execute(['revert', Plan.TargetHash]);
          Result := RecoveryResult.Success;
        end;
      rsBranchRestore:
        begin
          // Create/restore branch
          RecoveryResult := TGitRepository.CreateBranch('restored-branch', Plan.TargetHash);
          Result := RecoveryResult.Success;
        end;
    end;
    
    if Result then
    begin
      FCurrentStage := psCompleted;
      FMessage := AnsiString('Recovery completed successfully! Backup created at: ') + Plan.BackupBranchName;
    end
    else
    begin
      FCurrentStage := psError;
      FMessage := AnsiString('Recovery failed: ') + RecoveryResult.ErrorMessage;
    end;
  except
    on E: Exception do
    begin
      FCurrentStage := psError;
      FMessage := AnsiString('Recovery failed: ') + E.Message;
      Result := False;
    end;
  end;
end;

function TUndoModel.GenerateBackupBranchName: string;
begin
  Result := 'gitpal-backup-' + FormatDateTime('yyyymmdd-hhnnss', Now);
end;

function TUndoModel.GetStrategyDescription(Strategy: TRecoveryStrategy): string;
begin
  case Strategy of
    rsReset: Result := 'Git Reset (moves HEAD to target commit)';
    rsRevert: Result := 'Git Revert (creates new commit that undoes changes)';
    rsCherryPick: Result := 'Remove Cherry-pick (reverts specific commit)';
    rsBranchRestore: Result := 'Branch Restoration (recreates branch from reflog)';
  else
    Result := 'Unknown Strategy';
  end;
end;

function TUndoModel.GetRiskLevelDisplay(const RiskLevel: string): string;
begin
  if RiskLevel = 'high' then
    Result := '🔴 HIGH - May lose uncommitted work'
  else if RiskLevel = 'medium' then
    Result := '🟡 MEDIUM - May affect commit history'
  else if RiskLevel = 'low' then
    Result := '🟢 LOW - Safe operation'
  else
    Result := '⚪ UNKNOWN';
end;

function TUndoModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  NewList: bobacomponents.TList;
  SelectionMsg: bobacomponents.TListSelectionMsg;
begin
  // Handle list selection messages
  if Msg is bobacomponents.TListSelectionMsg then
  begin
    HandleListSelection(bobacomponents.TListSelectionMsg(Msg));
    Result.Model := Self;
    Result.Cmd := nil;
    Exit;
  end;
  
  // Handle async progress updates
  if Msg is TAsyncUndoProgressUpdateMsg then
  begin
    HandleAsyncProgressUpdate(TAsyncUndoProgressUpdateMsg(Msg));
    Result.Model := Self;
    Result.Cmd := nil;
    Exit;
  end;
  
  // Handle async result messages
  if Msg is TAsyncUndoResultMsg then
  begin
    HandleAsyncResult(TAsyncUndoResultMsg(Msg));
    Result.Model := Self;
    Result.Cmd := nil;
    Exit;
  end;
  
  // Handle spinner updates
  if Msg is bobaui.TTickMsg then
  begin
    FSpinner.Update(Msg);
    if FShowingSelection and Assigned(FOperationList) then
      FOperationList.Update(Msg);
    Result.Model := Self;
    Result.Cmd := nil;
    Exit;
  end;
  
  // Handle window size updates
  if Msg is bobaui.TWindowSizeMsg then
  begin
    FTerminalWidth := bobaui.TWindowSizeMsg(Msg).Width;
    FTerminalHeight := bobaui.TWindowSizeMsg(Msg).Height;
    if Assigned(FOperationList) then
    begin
      FOperationList.Width := FTerminalWidth - 4;
      FOperationList.Height := Min(10, FTerminalHeight - 10);
    end;
    Result.Model := Self;
    Result.Cmd := nil;
    Exit;
  end;
  
  // Handle keyboard input
  if Msg is bobaui.TKeyMsg then
  begin
    with bobaui.TKeyMsg(Msg) do
      begin
        // Exit on 'q' or Ctrl+C
        if (Key = 'q') or (Key = #3) then
        begin
          Result.Model := Self;
          Result.Cmd := bobaui.TQuitCommand.Create;
          Exit;
        end;
        
        // If showing selection, handle list navigation
        if FShowingSelection and Assigned(FOperationList) then
        begin
          // Handle Enter key to confirm selection
          if (Key = #13) and (FSelectedIndex >= 0) and (FSelectedIndex < Length(FUndoableEntries)) then
          begin
            StartRecoveryProcess(FUndoableEntries[FSelectedIndex]);
            Result.Model := Self;
            Result.Cmd := nil;
            Exit;
          end;
          
          // Pass other keys to the list for navigation
          NewList := FOperationList.Update(Msg);
          if NewList <> FOperationList then
          begin
            FOperationList.Free;
            FOperationList := NewList;
            
            // Check if list has a pending selection
            if NewList.HasPendingSelection then
            begin
              SelectionMsg := NewList.GetPendingSelection;
              HandleListSelection(SelectionMsg);
              NewList.ClearPendingSelection;
            end;
          end;
          Result.Model := Self;
          Result.Cmd := nil;
          Exit;
        end;
        
        // If showing confirmation, handle confirmation keys
        if FShowingConfirmation and (FCurrentStage = psAwaitingConfirmation) then
        begin
          if (Key = 'y') or (Key = 'Y') then
          begin
            // User confirmed, execute recovery
            FShowingConfirmation := False;
            ExecuteRecovery(FRecoveryPlan);
            Result.Model := Self;
            Result.Cmd := nil;
            Exit;
          end
          else if (Key = 'n') or (Key = 'N') then
          begin
            // User declined, go back to selection
            FCurrentStage := psAwaitingSelection;
            FMessage := AnsiString('Recovery cancelled. Select another operation or press q to quit.');
            FShowingConfirmation := False;
            FShowingSelection := True;
            Result.Model := Self;
            Result.Cmd := nil;
            Exit;
          end;
        end;
      end;
  end;
  
  Result.Model := Self;
  Result.Cmd := nil;
end;

function TUndoModel.View: string;
var
  Lines: TStringList;
  StatusIcon: AnsiString;
  SelectedEntry: TReflogEntry;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(AnsiString(''));
    Lines.Add(AnsiString('🔄 gitpal undo'));
    Lines.Add(AnsiString(''));
    
    if FCustomPrompt <> '' then
    begin
      Lines.Add(AnsiString('Custom prompt: ') + FCustomPrompt);
      Lines.Add(AnsiString(''));
    end;
    
    // Show different views based on current stage
    if FShowingConfirmation and (FCurrentStage = psAwaitingConfirmation) then
    begin
      // Show recovery confirmation interface
      Lines.Add(AnsiString('🔍 Recovery Plan Review'));
      Lines.Add(AnsiString(''));
      Lines.Add(AnsiString('Strategy: ') + GetStrategyDescription(FRecoveryPlan.Strategy));
      Lines.Add(AnsiString('Target: ') + Copy(FRecoveryPlan.TargetHash, 1, 8));
      Lines.Add(AnsiString('Backup Branch: ') + FRecoveryPlan.BackupBranchName);
      Lines.Add(AnsiString('Risk Level: ') + GetRiskLevelDisplay(FRecoveryPlan.RiskLevel));
      Lines.Add(AnsiString(''));
      Lines.Add(AnsiString('Impact: ') + FRecoveryPlan.ImpactDescription);
      
      if Length(FRecoveryPlan.AffectedFiles) > 0 then
      begin
        Lines.Add(AnsiString(''));
        Lines.Add(AnsiString('Affected Files: ') + IntToStr(Length(FRecoveryPlan.AffectedFiles)));
        // Show first few files
        for I := 0 to Min(4, High(FRecoveryPlan.AffectedFiles)) do
        begin
          if FRecoveryPlan.AffectedFiles[I] <> '' then
            Lines.Add(AnsiString('  - ') + FRecoveryPlan.AffectedFiles[I]);
        end;
        if Length(FRecoveryPlan.AffectedFiles) > 5 then
          Lines.Add(AnsiString('  ... and ') + IntToStr(Length(FRecoveryPlan.AffectedFiles) - 5) + AnsiString(' more'));
      end;
      
      Lines.Add(AnsiString(''));
      Lines.Add(AnsiString('⚠️  WARNING: This operation will modify your repository!'));
      Lines.Add(AnsiString('A backup branch will be created first for safety.'));
      Lines.Add(AnsiString(''));
      Lines.Add(AnsiString('Proceed with recovery? (y/n)'));
    end
    else if FShowingSelection and (FCurrentStage = psAwaitingSelection) then
    begin
      // Show interactive selection interface
      Lines.Add(AnsiString('✅ Analysis complete! Select an operation to undo:'));
      Lines.Add(AnsiString(''));
      
      // Show the operation list
      if Assigned(FOperationList) and (Length(FUndoableEntries) > 0) then
      begin
        Lines.Add(FOperationList.View);
        Lines.Add(AnsiString(''));
        
        // Show details of selected operation
        if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FUndoableEntries)) then
        begin
          SelectedEntry := FUndoableEntries[FSelectedIndex];
          Lines.Add(AnsiString('🔍 Operation Details:'));
          Lines.Add(AnsiString('Hash: ') + SelectedEntry.Hash);
          Lines.Add(AnsiString('Operation: ') + SelectedEntry.Operation);
          Lines.Add(AnsiString('Time: ') + SelectedEntry.Timestamp);
          
          if SelectedEntry.AIDescription <> '' then
          begin
            Lines.Add(AnsiString('Description: ') + SelectedEntry.AIDescription);
          end;
          
          if SelectedEntry.UndoImpact <> '' then
          begin
            Lines.Add(AnsiString('Undo Impact: ') + SelectedEntry.UndoImpact);
          end;
          
          if SelectedEntry.RiskWarning <> '' then
          begin
            Lines.Add(AnsiString('⚠️  Warning: ') + SelectedEntry.RiskWarning);
          end;
        end;
        
        Lines.Add(AnsiString(''));
        Lines.Add(AnsiString('Navigation: ↑/↓ to select, Enter to confirm, q to quit'));
      end
      else
      begin
        Lines.Add(AnsiString('No undoable operations found in recent git history.'));
        Lines.Add(AnsiString(''));
        Lines.Add(AnsiString('Press q to quit'));
      end;
    end
    else
    begin
      // Show progress stages with completion indicators
      
      // Loading reflog
      if FLoadingReflogCompleted then
        StatusIcon := AnsiString('✓')
      else if FCurrentStage = psLoadingReflog then
        StatusIcon := FSpinner.View
      else
        StatusIcon := AnsiString('☐');
      Lines.Add(StatusIcon + AnsiString(' 📊 Loading git reflog...'));
      
      // Analyzing entries
      if FAnalyzingEntriesCompleted then
        StatusIcon := AnsiString('✓')
      else if FCurrentStage = psAnalyzingEntries then
        StatusIcon := FSpinner.View
      else
        StatusIcon := AnsiString('☐');
      Lines.Add(StatusIcon + AnsiString(' 🔍 Analyzing reflog entries...'));
      
      // Processing batch
      if FProcessingBatchCompleted then
        StatusIcon := AnsiString('✓')
      else if FCurrentStage = psProcessingBatch then
        StatusIcon := FSpinner.View
      else
        StatusIcon := AnsiString('☐');
      Lines.Add(StatusIcon + AnsiString(' ⚙️  Processing entry batches...'));
      
      // Generating descriptions
      if FGeneratingDescriptionsCompleted then
        StatusIcon := AnsiString('✓')
      else if FCurrentStage = psGeneratingDescriptions then
        StatusIcon := FSpinner.View
      else
        StatusIcon := AnsiString('☐');
      Lines.Add(StatusIcon + AnsiString(' 🤖 Generating operation descriptions...'));
      
      // Current status message
      if FCurrentStage = psError then
      begin
        Lines.Add(AnsiString(''));
        Lines.Add(AnsiString('❌ ') + FMessage);
      end
      else
      begin
        Lines.Add(AnsiString(''));
        Lines.Add(AnsiString('Current: ') + FMessage);
      end;
      
      Lines.Add(AnsiString(''));
      Lines.Add(AnsiString('Press q to quit'));
    end;
    
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure RunUndoCommand(const CustomPrompt: string);
var
  GitResult: TGitResult;
  Model: TUndoModel;
  App: bobaui.TBobaUIProgram;
begin
  // Check if we're in a git repository
  if not TGitRepository.IsRepository then
  begin
    writeln('Error: Not in a git repository');
    Halt(1);
  end;
  
  // Test that we can access reflog
  GitResult := TGitRepository.GetReflog([]);
  if not GitResult.Success then
  begin
    writeln('Error: Cannot access git reflog: ', GitResult.ErrorMessage);
    Halt(1);
  end;
  
  // Create and run the TUI
  Model := TUndoModel.Create(CustomPrompt);
  App := bobaui.TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set the global program reference for async operations
  GProgram := App;
  
  try
    App.Run;
  finally
    GProgram := nil;
    App.Free;
  end;
end;

end.