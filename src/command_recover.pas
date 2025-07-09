unit command_recover;

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
  SyncObjs,
  DateUtils;

type
  // Forward declarations
  TAsyncRecoverOperation = class;
  TAsyncRecoverOperationThread = class;

  // Progress stages for recover operation
  TRecoverProgressStage = (
    psInitializing,          // Starting up
    psLoadingReflog,         // Reading git reflog
    psAnalyzingCommits,      // Finding orphaned commits
    psProcessingBatch,       // Processing current batch
    psGroupingCommits,       // AI grouping related commits
    psAwaitingSelection,     // User selecting what to recover
    psGeneratingPlan,        // AI generating recovery plan
    psAwaitingConfirmation,  // User confirming recovery plan
    psExecutingRecovery,     // Performing the recovery operation
    psCompleted,             // Success
    psError                  // Error occurred
  );

  // Async operation state enumeration
  TAsyncRecoverOperationState = (
    osIdle,               // No operation running
    osAnalyzing,          // Analyzing orphaned commits
    osGrouping,           // Grouping commits
    osGeneratingPlan,     // Generating recovery plan
    osExecutingRecovery,  // Executing recovery plan
    osCancelling          // Cancelling current operation
  );

  // Recovery strategy enumeration
  TRecoveryAction = (
    raCreateBranch,       // Create new branch with commits
    raCherryPick,         // Cherry-pick to current branch
    raShowDiffs,          // Show diffs first
    raSkip                // Skip this group
  );

  // Orphaned commit data structure
  TOrphanedCommit = record
    Hash: string;
    ShortHash: string;
    Message: string;
    AuthorDate: TDateTime;
    CommitterDate: TDateTime;
    Author: string;
    Files: array of string;
    Insertions: Integer;
    Deletions: Integer;
  end;

  // Commit group data structure
  TCommitGroup = record
    GroupName: string;
    Description: string;
    Commits: array of TOrphanedCommit;
    Confidence: string; // 'high', 'medium', 'low'
    SuggestedBranchName: string;
    TotalCommits: Integer;
    DateRange: string;
  end;

  // Recovery plan data structure
  TRecoveryPlan = record
    Action: TRecoveryAction;
    TargetGroup: TCommitGroup;
    BranchName: string;
    ImpactDescription: string;
    CommitsToRecover: array of string;
    RequiresConfirmation: Boolean;
    // AI-generated detailed plan
    PlanTitle: string;
    PlanDescription: string;
    Steps: array of string;
    RiskWarnings: array of string;
    ExpectedOutcome: string;
  end;

  // Main recover model
  TRecoverModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FList: bobacomponents.TList;
    FSpinner: bobacomponents.TSpinner;
    FCustomPrompt: string;
    FProviderOverride: string;
    FCurrentStage: TRecoverProgressStage;
    FStatusMessage: string;
    
    // Async operation fields
    FAsyncState: TAsyncRecoverOperationState;
    FCurrentOperation: TAsyncRecoverOperation;
    FOperationId: string;
    FCursorHidden: Boolean;
    
    // Data fields
    FOrphanedCommits: array of TOrphanedCommit;
    FCommitGroups: array of TCommitGroup;
    FSelectedGroupIndex: Integer;
    FRecoveryPlan: TRecoveryPlan;
    FRecoveryExecuted: Boolean;
    FRecoverySuccessful: Boolean;
    FRecoveryErrorMessage: string;
    FHasRecoveryPlan: Boolean;
    FPlanList: bobacomponents.TList;
    
    procedure StartRecoverAnalysis;
    procedure StartRecoveryPlanGeneration(const GroupIndex: Integer);
    procedure StartRecoveryExecution;
    procedure CancelCurrentOperation;
    function GenerateOperationId: string;
    function FormatCommitGroup(const Group: TCommitGroup; Index: Integer): string;
    function FormatRecoveryPlan(const Plan: TRecoveryPlan): string;
  public
    constructor Create; overload;
    constructor Create(const ACustomPrompt: string); overload;
    constructor Create(const ACustomPrompt: string; const AProviderOverride: string); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

  // Async operation base class
  TAsyncRecoverOperation = class
  private
    FThread: TAsyncRecoverOperationThread;
    FOperationId: string;
    FIsRunning: Boolean;
    FCS: TCriticalSection;
  protected
    // Override this method to implement the actual operation
    function ExecuteOperation: bobaui.TMsg; virtual; abstract;
  public
    constructor Create(const AOperationId: string);
    destructor Destroy; override;
    procedure Start;
    procedure Cancel;
    function IsRunning: Boolean;
    property OperationId: string read FOperationId;
  end;

  // Background thread for async operations
  TAsyncRecoverOperationThread = class(TThread)
  private
    FOperation: TAsyncRecoverOperation;
    FOperationId: string;
  protected
    procedure Execute; override;
    procedure PostResultToMainThread(AResultMsg: bobaui.TMsg);
  public
    constructor Create(AOperation: TAsyncRecoverOperation; const AOperationId: string);
  end;

  // Specific async operation for commit recovery analysis
  TAsyncCommitRecoveryAnalysis = class(TAsyncRecoverOperation)
  private
    FCustomPrompt: string;
    FProviderOverride: string;
    FOrphanedCommits: array of TOrphanedCommit;
    FCommitGroups: array of TCommitGroup;
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ACustomPrompt: string; const AProviderOverride: string = '');
  end;

  // Async operation for recovery plan generation
  TAsyncRecoveryPlanGeneration = class(TAsyncRecoverOperation)
  private
    FCommitGroup: TCommitGroup;
    FCustomPrompt: string;
    FProviderOverride: string;
    FRecoveryPlan: TRecoveryPlan;
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ACommitGroup: TCommitGroup; const ACustomPrompt: string; const AProviderOverride: string = '');
  end;

  // Async operation for recovery plan execution
  TAsyncRecoveryExecution = class(TAsyncRecoverOperation)
  private
    FRecoveryPlan: TRecoveryPlan;
    FExecutionResult: Boolean;
    FExecutionError: string;
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ARecoveryPlan: TRecoveryPlan);
  end;

  // Progress update message
  TAsyncRecoverProgressUpdateMsg = class(bobaui.TMsg)
  private
    FOperationId: string;
    FStage: TRecoverProgressStage;
    FStatusMessage: string;
  public
    constructor Create(const AOperationId: string; AStage: TRecoverProgressStage; const AStatusMessage: string);
    property OperationId: string read FOperationId;
    property Stage: TRecoverProgressStage read FStage;
    property StatusMessage: string read FStatusMessage;
  end;

  // Commit analysis completed message
  TAsyncCommitAnalysisCompletedMsg = class(bobaui.TMsg)
  public
    OperationId: string;
    Success: Boolean;
    ErrorMessage: string;
    OrphanedCommits: array of TOrphanedCommit;
    CommitGroups: array of TCommitGroup;
    constructor Create(const AOperationId: string; const AOrphanedCommits: array of TOrphanedCommit; const ACommitGroups: array of TCommitGroup);
    constructor CreateError(const AOperationId: string; const AErrorMessage: string);
  end;

  // Recovery plan generation completed message
  TAsyncRecoveryPlanCompletedMsg = class(bobaui.TMsg)
  public
    OperationId: string;
    Success: Boolean;
    ErrorMessage: string;
    RecoveryPlan: TRecoveryPlan;
    constructor Create(const AOperationId: string; const ARecoveryPlan: TRecoveryPlan);
    constructor CreateError(const AOperationId: string; const AErrorMessage: string);
  end;

  // Recovery execution completed message
  TAsyncRecoveryExecutionCompletedMsg = class(bobaui.TMsg)
  public
    OperationId: string;
    Success: Boolean;
    ErrorMessage: string;
    constructor Create(const AOperationId: string; ASuccess: Boolean; const AErrorMessage: string = '');
  end;


// Public functions
procedure RunRecoverCommand(const CustomPrompt: string = ''; const ProviderOverride: string = '');

implementation

// Global variables for thread communication and working data
var
  GProgram: TBobaUIProgram = nil;
  GWorkingOrphanedCommits: array of TOrphanedCommit;
  GWorkingCommitGroups: array of TCommitGroup;

// Forward declarations
procedure FindOrphanedCommits; forward;
procedure GroupCommitsWithAI(const CustomPrompt: string; const ProviderOverride: string); forward;
function GenerateRecoveryPlan(const CommitGroup: TCommitGroup; const CustomPrompt: string; const ProviderOverride: string): TRecoveryPlan; forward;
function ExecuteRecovery(const Plan: TRecoveryPlan): Boolean; forward;
procedure ShowCommitDiffs(const CommitGroup: TCommitGroup); forward;

constructor TAsyncRecoverProgressUpdateMsg.Create(const AOperationId: string; AStage: TRecoverProgressStage; const AStatusMessage: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FStage := AStage;
  FStatusMessage := AStatusMessage;
end;

constructor TAsyncCommitAnalysisCompletedMsg.Create(const AOperationId: string; const AOrphanedCommits: array of TOrphanedCommit; const ACommitGroups: array of TCommitGroup);
var
  i: Integer;
begin
  inherited Create;
  OperationId := AOperationId;
  Success := True;
  ErrorMessage := AnsiString('');
  
  // Copy orphaned commits
  SetLength(OrphanedCommits, Length(AOrphanedCommits));
  for i := 0 to High(AOrphanedCommits) do
    OrphanedCommits[i] := AOrphanedCommits[i];
    
  // Copy commit groups
  SetLength(CommitGroups, Length(ACommitGroups));
  for i := 0 to High(ACommitGroups) do
    CommitGroups[i] := ACommitGroups[i];
end;

constructor TAsyncCommitAnalysisCompletedMsg.CreateError(const AOperationId: string; const AErrorMessage: string);
begin
  inherited Create;
  OperationId := AOperationId;
  Success := False;
  ErrorMessage := AErrorMessage;
  SetLength(OrphanedCommits, 0);
  SetLength(CommitGroups, 0);
end;

constructor TAsyncRecoveryPlanCompletedMsg.Create(const AOperationId: string; const ARecoveryPlan: TRecoveryPlan);
begin
  inherited Create;
  OperationId := AOperationId;
  Success := True;
  ErrorMessage := AnsiString('');
  RecoveryPlan := ARecoveryPlan;
end;

constructor TAsyncRecoveryPlanCompletedMsg.CreateError(const AOperationId: string; const AErrorMessage: string);
begin
  inherited Create;
  OperationId := AOperationId;
  Success := False;
  ErrorMessage := AErrorMessage;
  FillChar(RecoveryPlan, SizeOf(RecoveryPlan), 0);
end;

constructor TAsyncRecoveryExecutionCompletedMsg.Create(const AOperationId: string; ASuccess: Boolean; const AErrorMessage: string);
begin
  inherited Create;
  OperationId := AOperationId;
  Success := ASuccess;
  ErrorMessage := AErrorMessage;
end;


// TAsyncRecoverOperation implementations
constructor TAsyncRecoverOperation.Create(const AOperationId: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FIsRunning := False;
  FThread := nil;
  FCS := TCriticalSection.Create;
end;

destructor TAsyncRecoverOperation.Destroy;
begin
  try
    Cancel;
    FCS.Free;
    inherited Destroy;
  except
    // Silently ignore cleanup exceptions
  end;
end;

procedure TAsyncRecoverOperation.Start;
begin
  FCS.Enter;
  try
    if not FIsRunning then
    begin
      FIsRunning := True;
      FThread := TAsyncRecoverOperationThread.Create(Self, FOperationId);
      FThread.Start;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TAsyncRecoverOperation.Cancel;
begin
  FCS.Enter;
  try
    if FIsRunning and Assigned(FThread) then
    begin
      FThread.Terminate;
      FThread.WaitFor;
      Sleep(10); // Small delay to ensure thread cleanup is complete
      FThread.Free;
      FThread := nil;
      FIsRunning := False;
    end;
  finally
    FCS.Leave;
  end;
end;

function TAsyncRecoverOperation.IsRunning: Boolean;
begin
  FCS.Enter;
  try
    Result := FIsRunning;
  finally
    FCS.Leave;
  end;
end;

// TAsyncRecoverOperationThread implementations
constructor TAsyncRecoverOperationThread.Create(AOperation: TAsyncRecoverOperation; const AOperationId: string);
begin
  inherited Create(False); // Create suspended
  FOperation := AOperation;
  FOperationId := AOperationId;
  FreeOnTerminate := False; // We'll manage the lifetime manually
end;

procedure TAsyncRecoverOperationThread.PostResultToMainThread(AResultMsg: bobaui.TMsg);
begin
  try
    // Use BobaUI's Send() method for thread-safe message sending
    if Assigned(GProgram) and Assigned(AResultMsg) then
    begin
      GProgram.Send(AResultMsg);
    end
    else
    begin
      if Assigned(AResultMsg) then
        AResultMsg.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(AResultMsg) then
        AResultMsg.Free;
    end;
  end;
end;

procedure TAsyncRecoverOperationThread.Execute;
var
  ResultMsg: bobaui.TMsg;
begin
  try
    if not Terminated then
    begin
      // Execute the actual operation
      ResultMsg := FOperation.ExecuteOperation;
      
      // Post result back to main thread
      if not Terminated and Assigned(ResultMsg) then
      begin
        PostResultToMainThread(ResultMsg);
      end
      else
      begin
        if Assigned(ResultMsg) then
          ResultMsg.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      // Create error message
      ResultMsg := TAsyncCommitAnalysisCompletedMsg.CreateError(FOperationId, 'Thread exception: ' + E.Message);
      PostResultToMainThread(ResultMsg);
    end;
  end;
  
  // Mark operation as no longer running
  FOperation.FCS.Enter;
  try
    FOperation.FIsRunning := False;
  finally
    FOperation.FCS.Leave;
  end;
end;

// TAsyncCommitRecoveryAnalysis implementations
constructor TAsyncCommitRecoveryAnalysis.Create(const AOperationId: string; const ACustomPrompt: string; const AProviderOverride: string);
begin
  inherited Create(AOperationId);
  FCustomPrompt := ACustomPrompt;
  FProviderOverride := AProviderOverride;
  SetLength(FOrphanedCommits, 0);
  SetLength(FCommitGroups, 0);
end;

constructor TAsyncRecoveryPlanGeneration.Create(const AOperationId: string; const ACommitGroup: TCommitGroup; const ACustomPrompt: string; const AProviderOverride: string);
begin
  inherited Create(AOperationId);
  FCommitGroup := ACommitGroup;
  FCustomPrompt := ACustomPrompt;
  FProviderOverride := AProviderOverride;
  FillChar(FRecoveryPlan, SizeOf(FRecoveryPlan), 0);
end;

constructor TAsyncRecoveryExecution.Create(const AOperationId: string; const ARecoveryPlan: TRecoveryPlan);
begin
  inherited Create(AOperationId);
  FRecoveryPlan := ARecoveryPlan;
  FExecutionResult := False;
  FExecutionError := AnsiString('');
end;

function TAsyncCommitRecoveryAnalysis.ExecuteOperation: bobaui.TMsg;
var
  ProgressMsg: TAsyncRecoverProgressUpdateMsg;
begin
  try
    DebugLog('[TAsyncCommitRecoveryAnalysis.ExecuteOperation] Starting commit recovery analysis');
    
    // Send progress update: Loading reflog
    if Assigned(GProgram) then
    begin
      ProgressMsg := TAsyncRecoverProgressUpdateMsg.Create(FOperationId, psLoadingReflog, 'Reading git reflog...');
      GProgram.Send(ProgressMsg);
    end;
    
    // Find orphaned commits
    FindOrphanedCommits;
    FOrphanedCommits := GWorkingOrphanedCommits;
    DebugLog('[TAsyncCommitRecoveryAnalysis.ExecuteOperation] Found ' + IntToStr(Length(FOrphanedCommits)) + ' orphaned commits');
    
    if Length(FOrphanedCommits) = 0 then
    begin
      Result := TAsyncCommitAnalysisCompletedMsg.CreateError(FOperationId, 'No orphaned commits found. All commits appear to be reachable from existing branches.');
      Exit;
    end;
    
    // Send progress update: Grouping commits
    if Assigned(GProgram) then
    begin
      ProgressMsg := TAsyncRecoverProgressUpdateMsg.Create(FOperationId, psGroupingCommits, 'Analyzing and grouping related commits...');
      GProgram.Send(ProgressMsg);
    end;
    
    // Group commits using AI
    GroupCommitsWithAI(FCustomPrompt, FProviderOverride);
    FCommitGroups := GWorkingCommitGroups;
    DebugLog('[TAsyncCommitRecoveryAnalysis.ExecuteOperation] Created ' + IntToStr(Length(FCommitGroups)) + ' commit groups');
    
    Result := TAsyncCommitAnalysisCompletedMsg.Create(FOperationId, FOrphanedCommits, FCommitGroups);
  except
    on E: Exception do
    begin
      DebugLog('[TAsyncCommitRecoveryAnalysis.ExecuteOperation] Exception: ' + E.ClassName + ': ' + E.Message);
      Result := TAsyncCommitAnalysisCompletedMsg.CreateError(FOperationId, 'Exception: ' + E.Message);
    end;
  end;
end;

function TAsyncRecoveryPlanGeneration.ExecuteOperation: bobaui.TMsg;
var
  ProgressMsg: TAsyncRecoverProgressUpdateMsg;
begin
  try
    DebugLog('[TAsyncRecoveryPlanGeneration.ExecuteOperation] Starting recovery plan generation');
    
    // Send progress update
    if Assigned(GProgram) then
    begin
      ProgressMsg := TAsyncRecoverProgressUpdateMsg.Create(FOperationId, psGeneratingPlan, 'Generating recovery plan with AI...');
      GProgram.Send(ProgressMsg);
    end;
    
    // Generate recovery plan using AI
    FRecoveryPlan := GenerateRecoveryPlan(FCommitGroup, FCustomPrompt, FProviderOverride);
    DebugLog('[TAsyncRecoveryPlanGeneration.ExecuteOperation] Generated recovery plan: ' + FRecoveryPlan.PlanTitle);
    
    Result := TAsyncRecoveryPlanCompletedMsg.Create(FOperationId, FRecoveryPlan);
  except
    on E: Exception do
    begin
      DebugLog('[TAsyncRecoveryPlanGeneration.ExecuteOperation] Exception: ' + E.ClassName + ': ' + E.Message);
      Result := TAsyncRecoveryPlanCompletedMsg.CreateError(FOperationId, 'Exception: ' + E.Message);
    end;
  end;
end;

function TAsyncRecoveryExecution.ExecuteOperation: bobaui.TMsg;
var
  ProgressMsg: TAsyncRecoverProgressUpdateMsg;
begin
  try
    DebugLog('[TAsyncRecoveryExecution.ExecuteOperation] Starting recovery execution');
    
    // Send progress update
    if Assigned(GProgram) then
    begin
      ProgressMsg := TAsyncRecoverProgressUpdateMsg.Create(FOperationId, psExecutingRecovery, 'Executing recovery plan...');
      GProgram.Send(ProgressMsg);
    end;
    
    // Execute recovery plan
    FExecutionResult := ExecuteRecovery(FRecoveryPlan);
    
    if FExecutionResult then
    begin
      DebugLog('[TAsyncRecoveryExecution.ExecuteOperation] Recovery execution successful');
      Result := TAsyncRecoveryExecutionCompletedMsg.Create(FOperationId, True);
    end
    else
    begin
      DebugLog('[TAsyncRecoveryExecution.ExecuteOperation] Recovery execution failed: ' + FExecutionError);
      Result := TAsyncRecoveryExecutionCompletedMsg.Create(FOperationId, False, FExecutionError);
    end;
  except
    on E: Exception do
    begin
      DebugLog('[TAsyncRecoveryExecution.ExecuteOperation] Exception: ' + E.ClassName + ': ' + E.Message);
      Result := TAsyncRecoveryExecutionCompletedMsg.Create(FOperationId, False, 'Exception: ' + E.Message);
    end;
  end;
end;

constructor TRecoverModel.Create;
begin
  inherited Create;
  FTerminalWidth := 0;
  FTerminalHeight := 0;
  FCustomPrompt := AnsiString('');
  FProviderOverride := AnsiString('');
  FCurrentStage := psInitializing;
  FStatusMessage := AnsiString('Initializing...');
  
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  
  // Initialize data fields
  SetLength(FOrphanedCommits, 0);
  SetLength(FCommitGroups, 0);
  FSelectedGroupIndex := -1;
  FillChar(FRecoveryPlan, SizeOf(FRecoveryPlan), 0);
  FRecoveryExecuted := False;
  FRecoverySuccessful := False;
  FRecoveryErrorMessage := AnsiString('');
  FHasRecoveryPlan := False;
  
  FList := bobacomponents.TList.Create;
  FList.Width := 80;
  FList.Height := 10;
  FList.ShowBorder := false;
  FList.SelectedColor := bobastyle.cBrightBlue;
  
  FPlanList := bobacomponents.TList.Create;
  FPlanList.AddItem(AnsiString('Accept'));
  FPlanList.AddItem(AnsiString('Decline'));
  FPlanList.AddItem(AnsiString('Show diffs'));
  FPlanList.Width := 30;
  FPlanList.Height := 5;
  FPlanList.ShowBorder := false;
  FPlanList.SelectedColor := bobastyle.cBrightBlue;
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;

constructor TRecoverModel.Create(const ACustomPrompt: string);
begin
  Create;
  FCustomPrompt := ACustomPrompt;
end;

constructor TRecoverModel.Create(const ACustomPrompt: string; const AProviderOverride: string);
begin
  Create(ACustomPrompt);
  FProviderOverride := AProviderOverride;
end;

destructor TRecoverModel.Destroy;
begin
  try
    // Cancel any running async operation
    if Assigned(FCurrentOperation) then
    begin
      FCurrentOperation.Cancel;
      FCurrentOperation.Free;
      FCurrentOperation := nil;
    end;
    if Assigned(FList) then
    begin
      FList.Free;
      FList := nil;
    end;
    if Assigned(FPlanList) then
    begin
      FPlanList.Free;
      FPlanList := nil;
    end;
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

function TRecoverModel.GenerateOperationId: string;
begin
  Result := 'recover_op_' + IntToStr(GetTickCount64);
end;

procedure TRecoverModel.StartRecoverAnalysis;
begin
  if FAsyncState = osIdle then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osAnalyzing;
    FCurrentStage := psLoadingReflog;
    FStatusMessage := 'Starting commit recovery analysis...';
    
    // Create and start the async operation
    FCurrentOperation := TAsyncCommitRecoveryAnalysis.Create(FOperationId, FCustomPrompt, FProviderOverride);
    FCurrentOperation.Start;
  end;
end;

procedure TRecoverModel.StartRecoveryPlanGeneration(const GroupIndex: Integer);
begin
  if (FAsyncState = osIdle) and (GroupIndex >= 0) and (GroupIndex < Length(FCommitGroups)) then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osGeneratingPlan;
    FCurrentStage := psGeneratingPlan;
    FStatusMessage := 'Generating recovery plan...';
    FSelectedGroupIndex := GroupIndex;
    
    // Create and start the async operation
    FCurrentOperation := TAsyncRecoveryPlanGeneration.Create(FOperationId, FCommitGroups[GroupIndex], FCustomPrompt, FProviderOverride);
    FCurrentOperation.Start;
  end;
end;

procedure TRecoverModel.StartRecoveryExecution;
begin
  if (FAsyncState = osIdle) and FHasRecoveryPlan then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osExecutingRecovery;
    FCurrentStage := psExecutingRecovery;
    FStatusMessage := 'Executing recovery plan...';
    
    // Create and start the async operation
    FCurrentOperation := TAsyncRecoveryExecution.Create(FOperationId, FRecoveryPlan);
    FCurrentOperation.Start;
  end;
end;

procedure TRecoverModel.CancelCurrentOperation;
begin
  if Assigned(FCurrentOperation) then
  begin
    FCurrentOperation.Cancel;
    FCurrentOperation.Free;
    FCurrentOperation := nil;
  end;
  FAsyncState := osIdle;
end;

function TRecoverModel.FormatCommitGroup(const Group: TCommitGroup; Index: Integer): string;
var
  ConfidenceIndicator: string;
  CommitCountStr: string;
begin
  // Choose confidence indicator
  case Group.Confidence of
    'high': ConfidenceIndicator := '🟢';
    'medium': ConfidenceIndicator := '🟡';
    'low': ConfidenceIndicator := '🔴';
    else ConfidenceIndicator := '⚪';
  end;
  
  // Format commit count
  if Group.TotalCommits = 1 then
    CommitCountStr := '1 commit'
  else
    CommitCountStr := IntToStr(Group.TotalCommits) + ' commits';
  
  Result := Format('%s %s (%s) - %s', [
    ConfidenceIndicator,
    Group.GroupName,
    CommitCountStr,
    Group.Description
  ]);
end;

function TRecoverModel.FormatRecoveryPlan(const Plan: TRecoveryPlan): string;
var
  Sections: array of string;
  i: Integer;
  StepText: string;
  WarningText: string;
begin
  SetLength(Sections, 0);
  
  // Plan title and description
  SetLength(Sections, Length(Sections) + 1);
  Sections[High(Sections)] := bobastyle.ColorText('📋 ' + Plan.PlanTitle, bobastyle.cBrightBlue);
  
  if Plan.PlanDescription <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := Plan.PlanDescription;
  end;
  
  // Steps
  if Length(Plan.Steps) > 0 then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText('Steps:', bobastyle.cBrightWhite);
    
    for i := 0 to High(Plan.Steps) do
    begin
      SetLength(Sections, Length(Sections) + 1);
      StepText := Format('%d. %s', [i + 1, Plan.Steps[i]]);
      Sections[High(Sections)] := StepText;
    end;
  end;
  
  // Risk warnings
  if Length(Plan.RiskWarnings) > 0 then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText('⚠️  Warnings:', bobastyle.cBrightYellow);
    
    for i := 0 to High(Plan.RiskWarnings) do
    begin
      SetLength(Sections, Length(Sections) + 1);
      WarningText := '• ' + Plan.RiskWarnings[i];
      Sections[High(Sections)] := bobastyle.ColorText(WarningText, bobastyle.cBrightYellow);
    end;
  end;
  
  // Expected outcome
  if Plan.ExpectedOutcome <> '' then
  begin
    SetLength(Sections, Length(Sections) + 2);
    Sections[High(Sections) - 1] := AnsiString('');
    Sections[High(Sections)] := bobastyle.ColorText('Expected result: ' + Plan.ExpectedOutcome, bobastyle.cBrightGreen);
  end;
  
  Result := bobastyle.JoinVertical(Sections);
end;

function TRecoverModel.View: string;
var
  Style: bobastyle.TStyle;
  Sections: array of string;
  i: Integer;
  GroupStr: string;
begin
  // Handle initial state gracefully when terminal size is unknown
  if FTerminalWidth <= 0 then
  begin
    Result := AnsiString('Detecting terminal size...');
    Exit;
  end;
  
  // If we're still analyzing, show progress
  if FAsyncState <> osIdle then
  begin
    Result := FSpinner.View + AnsiString(' ') + FStatusMessage;
    Exit;
  end;
  
  // Show execution results if completed
  if FCurrentStage = psCompleted then
  begin
    if FRecoverySuccessful then
      Result := bobastyle.ColorText(AnsiString('✓ Recovery completed successfully!'), bobastyle.cBrightGreen)
    else
      Result := bobastyle.ColorText(AnsiString('✗ Recovery failed: ') + FRecoveryErrorMessage, bobastyle.cBrightRed);
    Exit;
  end;
  
  // If no commit groups found, show error or empty state
  if Length(FCommitGroups) = 0 then
  begin
    if FCurrentStage = psError then
      Result := bobastyle.ColorText(AnsiString('✗ ') + FRecoveryErrorMessage, bobastyle.cBrightRed)
    else
      Result := AnsiString('No orphaned commits found. All commits appear to be reachable from existing branches.');
    Exit;
  end;
  
  // Show recovery plan confirmation if we have a plan
  if FHasRecoveryPlan and (FCurrentStage = psAwaitingConfirmation) then
  begin
    SetLength(Sections, 4);
    Sections[0] := FormatRecoveryPlan(FRecoveryPlan);
    Sections[1] := AnsiString('');
    Sections[2] := AnsiString('Proceed with this recovery plan?');
    Sections[3] := FPlanList.View;
    
    Result := bobastyle.JoinVertical(Sections);
    Exit;
  end;
  
  // Show commit groups for selection
  SetLength(Sections, 4);
  Sections[0] := AnsiString('🔍 Found orphaned commits:');
  Sections[1] := AnsiString(''); // Empty line
  
  // Populate list with commit groups
  FList.ClearItems;
  for i := 0 to High(FCommitGroups) do
  begin
    GroupStr := FormatCommitGroup(FCommitGroups[i], i);
    FList.AddItem(GroupStr);
  end;
  
  Sections[2] := FList.View;
  Sections[3] := AnsiString('') + #10 + AnsiString('Select a group to recover (Enter to confirm, q to quit)');
  
  Result := bobastyle.JoinVertical(Sections);
end;

function TRecoverModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  ProgressMsg: TAsyncRecoverProgressUpdateMsg;
  AnalysisMsg: TAsyncCommitAnalysisCompletedMsg;
  RecoveryPlanMsg: TAsyncRecoveryPlanCompletedMsg;
  ExecutionMsg: TAsyncRecoveryExecutionCompletedMsg;
  ListSelectionMsg: bobacomponents.TListSelectionMsg;
  NewModel: TRecoverModel;
  NewList: bobacomponents.TList;
  NewSpinner: bobacomponents.TSpinner;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  // Handle async progress messages
  if Msg is TAsyncRecoverProgressUpdateMsg then
  begin
    ProgressMsg := TAsyncRecoverProgressUpdateMsg(Msg);
    if ProgressMsg.OperationId = FOperationId then
    begin
      FCurrentStage := ProgressMsg.Stage;
      FStatusMessage := ProgressMsg.StatusMessage;
    end;
    Exit;
  end;

  // Handle async analysis completion
  if Msg is TAsyncCommitAnalysisCompletedMsg then
  begin
    AnalysisMsg := TAsyncCommitAnalysisCompletedMsg(Msg);
    if AnalysisMsg.OperationId = FOperationId then
    begin
      CancelCurrentOperation; // Clean up
      
      if AnalysisMsg.Success then
      begin
        // Store the results
        FOrphanedCommits := AnalysisMsg.OrphanedCommits;
        FCommitGroups := AnalysisMsg.CommitGroups;
        FCurrentStage := psAwaitingSelection;
        FAsyncState := osIdle;
      end
      else
      begin
        // Error case
        FCurrentStage := psError;
        FRecoveryErrorMessage := AnalysisMsg.ErrorMessage;
        FAsyncState := osIdle;
      end;
    end;
    Exit;
  end;

  // Handle async recovery plan completion
  if Msg is TAsyncRecoveryPlanCompletedMsg then
  begin
    RecoveryPlanMsg := TAsyncRecoveryPlanCompletedMsg(Msg);
    if RecoveryPlanMsg.OperationId = FOperationId then
    begin
      CancelCurrentOperation; // Clean up
      
      if RecoveryPlanMsg.Success then
      begin
        // Store the recovery plan
        FRecoveryPlan := RecoveryPlanMsg.RecoveryPlan;
        FHasRecoveryPlan := True;
        FCurrentStage := psAwaitingConfirmation;
        FAsyncState := osIdle;
      end
      else
      begin
        // Error case
        FCurrentStage := psError;
        FRecoveryErrorMessage := RecoveryPlanMsg.ErrorMessage;
        FAsyncState := osIdle;
      end;
    end;
    Exit;
  end;

  // Handle async recovery execution completion
  if Msg is TAsyncRecoveryExecutionCompletedMsg then
  begin
    ExecutionMsg := TAsyncRecoveryExecutionCompletedMsg(Msg);
    if ExecutionMsg.OperationId = FOperationId then
    begin
      CancelCurrentOperation; // Clean up
      
      FRecoveryExecuted := True;
      FRecoverySuccessful := ExecutionMsg.Success;
      if not ExecutionMsg.Success then
        FRecoveryErrorMessage := ExecutionMsg.ErrorMessage;
      
      FCurrentStage := psCompleted;
      FAsyncState := osIdle;
      
      // Show result and quit
      Result.Cmd := bobaui.QuitCmd;
    end;
    Exit;
  end;

  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Handle 'q' to quit
    if (KeyMsg.Key = 'q') or (KeyMsg.Key = 'Q') then
    begin
      CancelCurrentOperation;
      Result.Cmd := bobaui.QuitCmd;
    end
    // Handle list navigation if we have commit groups or recovery plan
    else if ((Length(FCommitGroups) > 0) or FHasRecoveryPlan) and (FAsyncState = osIdle) then
    begin
      if FHasRecoveryPlan and (FCurrentStage = psAwaitingConfirmation) then
      begin
        // Handle plan confirmation list
        NewList := FPlanList.Update(Msg);
        if NewList <> FPlanList then
        begin
          NewModel := TRecoverModel.Create(FCustomPrompt, FProviderOverride);
          NewModel.FTerminalWidth := FTerminalWidth;
          NewModel.FTerminalHeight := FTerminalHeight;
          NewModel.FCommitGroups := FCommitGroups;
          NewModel.FOrphanedCommits := FOrphanedCommits;
          NewModel.FCurrentStage := FCurrentStage;
          NewModel.FAsyncState := FAsyncState;
          NewModel.FRecoveryPlan := FRecoveryPlan;
          NewModel.FHasRecoveryPlan := FHasRecoveryPlan;
          NewModel.FSelectedGroupIndex := FSelectedGroupIndex;
          NewModel.FPlanList.Free;
          NewModel.FPlanList := NewList;
          
          // Check if list has a pending selection
          if NewList.HasPendingSelection then
          begin
            Result.Cmd := bobacomponents.ListSelectionCmd(NewList.SelectedIndex, NewList.SelectedItem, NewList.ListId);
            NewList.ClearPendingSelection;
          end;
          
          Result.Model := NewModel;
        end;
      end
      else
      begin
        // Handle commit group selection list
        NewList := FList.Update(Msg);
        if NewList <> FList then
        begin
          NewModel := TRecoverModel.Create(FCustomPrompt, FProviderOverride);
          NewModel.FTerminalWidth := FTerminalWidth;
          NewModel.FTerminalHeight := FTerminalHeight;
          NewModel.FCommitGroups := FCommitGroups;
          NewModel.FOrphanedCommits := FOrphanedCommits;
          NewModel.FCurrentStage := FCurrentStage;
          NewModel.FAsyncState := FAsyncState;
          NewModel.FList.Free;
          NewModel.FList := NewList;
          
          // Check if list has a pending selection
          if NewList.HasPendingSelection then
          begin
            Result.Cmd := bobacomponents.ListSelectionCmd(NewList.SelectedIndex, NewList.SelectedItem, NewList.ListId);
            NewList.ClearPendingSelection;
          end;
          
          Result.Model := NewModel;
        end;
      end;
    end;
  end
  else if Msg is bobacomponents.TListSelectionMsg then
  begin
    ListSelectionMsg := bobacomponents.TListSelectionMsg(Msg);
    
    if FHasRecoveryPlan and (FCurrentStage = psAwaitingConfirmation) then
    begin
      // Handle recovery plan confirmation
      case ListSelectionMsg.SelectedIndex of
        0: // Accept
        begin
          // Execute recovery plan
          StartRecoveryExecution;
        end;
        1: // Decline
        begin
          Result.Cmd := bobaui.QuitCmd;
        end;
        2: // Show diffs
        begin
          // Show diffs for the commits in the group
          ShowCommitDiffs(FCommitGroups[FSelectedGroupIndex]);
          Result.Cmd := bobaui.QuitCmd;
        end;
      end;
    end
    else if (ListSelectionMsg.SelectedIndex >= 0) and (ListSelectionMsg.SelectedIndex < Length(FCommitGroups)) then
    begin
      // User selected a commit group - start recovery plan generation
      StartRecoveryPlanGeneration(ListSelectionMsg.SelectedIndex);
    end;
  end
  else if Msg is bobaui.TComponentTickMsg then
  begin
    if FAsyncState <> osIdle then  // Show spinner while analyzing
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
    
    // Adjust list size to terminal
    if Assigned(FList) then
    begin
      FList.Width := FTerminalWidth - 4;
      if FTerminalHeight - 8 < 15 then
        FList.Height := FTerminalHeight - 8
      else
        FList.Height := 15;
    end;
    
    // Hide cursor and start analysis on first window size message
    if not FCursorHidden then
    begin
      FCursorHidden := True;
      
      if FAsyncState = osIdle then
      begin
        StartRecoverAnalysis;
        Result.Cmd := bobaui.BatchCmd([bobaui.HideCursorCmd, FSpinner.Tick]);
      end
      else
      begin
        Result.Cmd := bobaui.HideCursorCmd;
      end;
    end;
  end;
end;

procedure FindOrphanedCommits;
var
  ReflogResult: git.TGitResult;
  BranchesResult: git.TGitResult;
  ReflogLines: TStringList;
  BranchCommits: TStringList;
  i: Integer;
  Line: string;
  Hash: string;
  Commit: TOrphanedCommit;
  CommitInfoResult: git.TGitResult;
  OrphanedList: array of TOrphanedCommit;
  OrphanedCount: Integer;
begin
  DebugLog('[FindOrphanedCommits] Starting search for orphaned commits');
  
  SetLength(GWorkingOrphanedCommits, 0);
  OrphanedCount := 0;
  SetLength(OrphanedList, 100); // Pre-allocate space
  
  // Get reflog entries
  ReflogResult := git.TGitRepository.GetReflog(['--all', '--format=%H %gD %gs', '-n', '100']);
  if not ReflogResult.Success then
  begin
    DebugLog('[FindOrphanedCommits] Failed to get reflog: ' + ReflogResult.ErrorMessage);
    Exit;
  end;
  
  // Get all commits reachable from current branches
  BranchesResult := git.TGitRepository.Execute(['rev-list', '--all']);
  if not BranchesResult.Success then
  begin
    DebugLog('[FindOrphanedCommits] Failed to get branch commits: ' + BranchesResult.ErrorMessage);
    Exit;
  end;
  
  ReflogLines := TStringList.Create;
  BranchCommits := TStringList.Create;
  try
    ReflogLines.Text := ReflogResult.Output;
    BranchCommits.Text := BranchesResult.Output;
    
    DebugLog('[FindOrphanedCommits] Found ' + IntToStr(ReflogLines.Count) + ' reflog entries');
    DebugLog('[FindOrphanedCommits] Found ' + IntToStr(BranchCommits.Count) + ' reachable commits');
    
    // Check each reflog entry
    for i := 0 to ReflogLines.Count - 1 do
    begin
      Line := Trim(ReflogLines[i]);
      if Line = '' then Continue;
      
      // Extract hash (first 40 characters)
      if Length(Line) >= 40 then
      begin
        Hash := Copy(Line, 1, 40);
        
        // Check if this commit is NOT in the branch commits list
        if BranchCommits.IndexOf(Hash) = -1 then
        begin
          DebugLog('[FindOrphanedCommits] Found orphaned commit: ' + Hash);
          
          // Get detailed commit information
          CommitInfoResult := git.TGitRepository.GetCommitInfo(Hash);
          if CommitInfoResult.Success then
          begin
            FillChar(Commit, SizeOf(Commit), 0);
            // Parse commit info - this is simplified, would need proper parsing
            Commit.Hash := Hash;
            Commit.ShortHash := Copy(Hash, 1, 8);
            Commit.Message := 'Commit message'; // TODO: Parse from CommitInfoResult.Output
            Commit.AuthorDate := Now; // TODO: Parse date
            Commit.CommitterDate := Now; // TODO: Parse date
            Commit.Author := 'Unknown'; // TODO: Parse author
            SetLength(Commit.Files, 0); // TODO: Parse files
            Commit.Insertions := 0;
            Commit.Deletions := 0;
            
            // Add to orphaned list
            if OrphanedCount >= Length(OrphanedList) then
              SetLength(OrphanedList, Length(OrphanedList) * 2);
            OrphanedList[OrphanedCount] := Commit;
            Inc(OrphanedCount);
          end;
        end;
      end;
    end;
    
    // Copy to result array
    SetLength(GWorkingOrphanedCommits, OrphanedCount);
    for i := 0 to OrphanedCount - 1 do
      GWorkingOrphanedCommits[i] := OrphanedList[i];
      
    DebugLog('[FindOrphanedCommits] Found ' + IntToStr(Length(GWorkingOrphanedCommits)) + ' orphaned commits');
  finally
    ReflogLines.Free;
    BranchCommits.Free;
  end;
end;

procedure GroupCommitsWithAI(const CustomPrompt: string; const ProviderOverride: string);
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  SystemPrompt: string;
  UserPrompt: string;
  CommitsInfo: string;
  i: Integer;
  Group: TCommitGroup;
begin
  DebugLog('[GroupCommitsWithAI] Starting AI grouping of ' + IntToStr(Length(GWorkingOrphanedCommits)) + ' commits');
  
  // For now, create a simple default group
  SetLength(GWorkingCommitGroups, 1);
  FillChar(Group, SizeOf(Group), 0);
  Group.GroupName := 'Orphaned Commits';
  Group.Description := 'Commits not reachable from any branch';
  Group.Confidence := 'medium';
  Group.SuggestedBranchName := 'recovered-commits';
  Group.TotalCommits := Length(GWorkingOrphanedCommits);
  Group.DateRange := 'Recent';
  SetLength(Group.Commits, Length(GWorkingOrphanedCommits));
  for i := 0 to High(GWorkingOrphanedCommits) do
    Group.Commits[i] := GWorkingOrphanedCommits[i];
  GWorkingCommitGroups[0] := Group;
  
  // TODO: Implement actual AI grouping
  DebugLog('[GroupCommitsWithAI] Created 1 default group with ' + IntToStr(Length(GWorkingOrphanedCommits)) + ' commits');
end;

function GenerateRecoveryPlan(const CommitGroup: TCommitGroup; const CustomPrompt: string; const ProviderOverride: string): TRecoveryPlan;
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  SystemPrompt: string;
  UserPrompt: string;
  CommitsInfo: string;
  i: Integer;
begin
  DebugLog('[GenerateRecoveryPlan] Starting recovery plan generation for group: ' + CommitGroup.GroupName);
  
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  
  // For now, create a simple default plan
  Result.Action := raCreateBranch;
  Result.TargetGroup := CommitGroup;
  Result.BranchName := 'recover-' + StringReplace(LowerCase(CommitGroup.GroupName), ' ', '-', [rfReplaceAll]) + '-' + FormatDateTime('yyyymmdd-hhnnss', Now);
  Result.RequiresConfirmation := True;
  
  // AI-generated plan details
  Result.PlanTitle := 'Recovery Plan for "' + CommitGroup.GroupName + '"';
  Result.PlanDescription := 'This plan will recover ' + IntToStr(CommitGroup.TotalCommits) + ' orphaned commits by creating a new branch.';
  
  // Build commit info for steps
  CommitsInfo := '';
  for i := 0 to High(CommitGroup.Commits) do
  begin
    if i > 0 then CommitsInfo := CommitsInfo + ', ';
    CommitsInfo := CommitsInfo + CommitGroup.Commits[i].ShortHash;
  end;
  
  // Generate detailed steps
  SetLength(Result.Steps, 3);
  Result.Steps[0] := 'git branch ' + Result.BranchName + ' HEAD';
  Result.Steps[1] := 'git cherry-pick ' + CommitsInfo;
  Result.Steps[2] := 'git log --oneline -n ' + IntToStr(CommitGroup.TotalCommits);
  
  // Add risk warnings
  SetLength(Result.RiskWarnings, 2);
  Result.RiskWarnings[0] := 'This will modify your current branch by adding the recovered commits';
  Result.RiskWarnings[1] := 'A backup branch will be created first for safety';
  
  // Expected outcome
  Result.ExpectedOutcome := 'The ' + IntToStr(CommitGroup.TotalCommits) + ' orphaned commits will be recovered to your current branch';
  
  // Set commits to recover
  SetLength(Result.CommitsToRecover, Length(CommitGroup.Commits));
  for i := 0 to High(CommitGroup.Commits) do
    Result.CommitsToRecover[i] := CommitGroup.Commits[i].Hash;
  
  // TODO: Implement actual AI generation using LLM
  DebugLog('[GenerateRecoveryPlan] Generated plan with ' + IntToStr(Length(Result.Steps)) + ' steps');
end;

function ExecuteRecovery(const Plan: TRecoveryPlan): Boolean;
var
  GitResult: git.TGitResult;
  BackupBranch: string;
  i: Integer;
  CommitHash: string;
  CurrentBranch: string;
begin
  Result := False;
  DebugLog('[ExecuteRecovery] Starting recovery execution for: ' + Plan.PlanTitle);
  
  try
    // Step 1: Get current branch name for backup
    GitResult := git.TGitRepository.Execute(['branch', '--show-current']);
    if not GitResult.Success then
    begin
      DebugLog('[ExecuteRecovery] Failed to get current branch: ' + GitResult.ErrorMessage);
      Exit;
    end;
    CurrentBranch := Trim(GitResult.Output);
    
    // Step 2: Create backup branch for safety
    BackupBranch := 'backup-before-recover-' + FormatDateTime('yyyymmdd-hhnnss', Now);
    DebugLog('[ExecuteRecovery] Creating backup branch: ' + BackupBranch);
    GitResult := git.TGitRepository.CreateBranch(BackupBranch, 'HEAD');
    if not GitResult.Success then
    begin
      DebugLog('[ExecuteRecovery] Failed to create backup branch: ' + GitResult.ErrorMessage);
      Exit;
    end;
    
    // Step 3: Execute recovery based on action type
    case Plan.Action of
      raCreateBranch:
      begin
        DebugLog('[ExecuteRecovery] Creating new branch: ' + Plan.BranchName);
        // Create new branch from current HEAD
        GitResult := git.TGitRepository.CreateBranch(Plan.BranchName, 'HEAD');
        if not GitResult.Success then
        begin
          DebugLog('[ExecuteRecovery] Failed to create recovery branch: ' + GitResult.ErrorMessage);
          Exit;
        end;
        
        // Switch to new branch
        GitResult := git.TGitRepository.CheckoutBranch(Plan.BranchName);
        if not GitResult.Success then
        begin
          DebugLog('[ExecuteRecovery] Failed to checkout recovery branch: ' + GitResult.ErrorMessage);
          Exit;
        end;
        
        // Cherry-pick all commits in the plan
        for i := 0 to High(Plan.CommitsToRecover) do
        begin
          CommitHash := Plan.CommitsToRecover[i];
          DebugLog('[ExecuteRecovery] Cherry-picking commit: ' + CommitHash);
          GitResult := git.TGitRepository.Execute(['cherry-pick', CommitHash]);
          if not GitResult.Success then
          begin
            DebugLog('[ExecuteRecovery] Failed to cherry-pick ' + CommitHash + ': ' + GitResult.ErrorMessage);
            // Try to abort the cherry-pick
            git.TGitRepository.Execute(['cherry-pick', '--abort']);
            // Switch back to original branch
            git.TGitRepository.CheckoutBranch(CurrentBranch);
            Exit;
          end;
        end;
      end;
      
      raCherryPick:
      begin
        DebugLog('[ExecuteRecovery] Cherry-picking to current branch');
        // Cherry-pick all commits to current branch
        for i := 0 to High(Plan.CommitsToRecover) do
        begin
          CommitHash := Plan.CommitsToRecover[i];
          DebugLog('[ExecuteRecovery] Cherry-picking commit: ' + CommitHash);
          GitResult := git.TGitRepository.Execute(['cherry-pick', CommitHash]);
          if not GitResult.Success then
          begin
            DebugLog('[ExecuteRecovery] Failed to cherry-pick ' + CommitHash + ': ' + GitResult.ErrorMessage);
            // Try to abort the cherry-pick
            git.TGitRepository.Execute(['cherry-pick', '--abort']);
            Exit;
          end;
        end;
      end;
      
      raShowDiffs:
      begin
        // This should not be reached in execution - diffs are shown before confirmation
        DebugLog('[ExecuteRecovery] Unexpected raShowDiffs action in execution');
        Exit;
      end;
      
      raSkip:
      begin
        DebugLog('[ExecuteRecovery] Skipping recovery as requested');
        Result := True;
        Exit;
      end;
    end;
    
    DebugLog('[ExecuteRecovery] Recovery execution completed successfully');
    Result := True;
    
  except
    on E: Exception do
    begin
      DebugLog('[ExecuteRecovery] Exception during recovery: ' + E.ClassName + ': ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure RunRecoverCommand(const CustomPrompt: string; const ProviderOverride: string);
var
  Prog: TBobaUIProgram;
  Model: TRecoverModel;
  RepoResult: git.TGitResult;
begin
  // Check if we're in a git repository
  if not git.TGitRepository.IsRepository('.') then
  begin
    writeln();
    writeln(bobastyle.ColorText('✗ Error: Not in a git repository', bobastyle.cBrightRed));
    writeln('  Please run this command from within a git repository.');
    Exit;
  end;
  
  // Check repository status
  RepoResult := git.TGitRepository.GetDiff([]);
  if not RepoResult.Success then
  begin
    writeln();
    writeln(bobastyle.ColorText('✗ Error: Cannot read repository status', bobastyle.cBrightRed));
    writeln('  ' + RepoResult.ErrorMessage);
    Exit;
  end;
  
  // Create model
  Model := TRecoverModel.Create(CustomPrompt, ProviderOverride);
  
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set the global program reference for async operations
  GProgram := Prog;
  
  try
    Prog.Run;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
  
  GProgram := nil;
  
  try
    Prog.Free;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
  
  // Add a small delay to ensure all background cleanup is complete
  Sleep(50);
end;

procedure ShowCommitDiffs(const CommitGroup: TCommitGroup);
var
  i: Integer;
  GitResult: git.TGitResult;
  CommitHash: string;
begin
  writeln();
  writeln(bobastyle.ColorText('📋 Diffs for "' + CommitGroup.GroupName + '" (' + IntToStr(CommitGroup.TotalCommits) + ' commits)', bobastyle.cBrightBlue));
  writeln();
  
  for i := 0 to High(CommitGroup.Commits) do
  begin
    CommitHash := CommitGroup.Commits[i].Hash;
    writeln(bobastyle.ColorText('📝 Commit ' + CommitGroup.Commits[i].ShortHash + ': ' + CommitGroup.Commits[i].Message, bobastyle.cBrightYellow));
    writeln(bobastyle.ColorText('────────────────────────────────────────────────────────────────', bobastyle.cBrightBlack));
    
    // Show the diff for this commit
    GitResult := git.TGitRepository.Execute(['show', '--format=', CommitHash]);
    if GitResult.Success then
    begin
      writeln(GitResult.Output);
    end
    else
    begin
      writeln(bobastyle.ColorText('Error getting diff for commit ' + CommitHash + ': ' + GitResult.ErrorMessage, bobastyle.cBrightRed));
    end;
    
    if i < High(CommitGroup.Commits) then
    begin
      writeln();
      writeln(bobastyle.ColorText('════════════════════════════════════════════════════════════════', bobastyle.cBrightBlack));
      writeln();
    end;
  end;
  
  writeln();
  writeln(bobastyle.ColorText('End of diffs for "' + CommitGroup.GroupName + '"', bobastyle.cBrightBlue));
end;

initialization

finalization
  try
    if Assigned(GProgram) then
    begin
      GProgram := nil;
    end;
  except
    // Silently ignore finalization exceptions
  end;

end.