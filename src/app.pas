program app;

{$mode objfpc}
{$codepage UTF8}
{$H+}


uses
  bobaui,
  bobastyle,
  bobacomponents,
  models,
  SysUtils,
  Process,
  Classes,
  SyncObjs;

const
  AppVersion = '0.0.6';  // Update this when tagging new releases

type
  // Forward declarations
  TAsyncOperation = class;
  TAsyncOperationThread = class;

  // Async operation state enumeration
  TAsyncOperationState = (
    osIdle,           // No operation running
    osGenerating,     // Generating commit message
    osCommitting,     // Executing git commit
    osCancelling      // Cancelling current operation
  );

  TCommitModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FList: bobacomponents.TList;
    FCommitMessage: string;
    FCommitExecuted: Boolean;
    FCommitSuccessful: Boolean;
    FCommitErrorMessage: string;
    FSpinner: bobacomponents.TSpinner;
    FGenerating: Boolean;
    FDiffContent: string;
    FCustomPrompt: string;
    FGenerationStarted: Boolean;
    // Async operation fields
    FAsyncState: TAsyncOperationState;
    FCurrentOperation: TAsyncOperation;
    FOperationId: string;
    FCursorHidden: Boolean;
    
    procedure StartCommitGeneration;
    procedure CancelCurrentOperation;
    function GenerateOperationId: string;
  public
    constructor Create; overload;
    constructor Create(const ACommitMessage: string); overload;
    constructor Create(const ADiffContent: string; const ACustomPrompt: string); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;
  
  // Custom message for when commit generation is complete
  TCommitGeneratedMsg = class(bobaui.TMsg)
  public
    CommitMessage: string;
    constructor Create(const ACommitMessage: string);
  end;

  // Base class for async operation results
  TAsyncResultMsg = class(bobaui.TMsg)
  private
    FOperationId: string;
    FSuccess: Boolean;
    FErrorMessage: string;
  public
    constructor Create(const AOperationId: string; ASuccess: Boolean; 
                     const AErrorMessage: string = '');
    property OperationId: string read FOperationId;
    property Success: Boolean read FSuccess;
    property ErrorMessage: string read FErrorMessage;
  end;

  // Enhanced commit generation result message
  TAsyncCommitGeneratedMsg = class(TAsyncResultMsg)
  private
    FCommitMessage: string;
  public
    constructor Create(const AOperationId: string; const ACommitMessage: string);
    constructor CreateError(const AOperationId: string; const AErrorMessage: string);
    property CommitMessage: string read FCommitMessage;
  end;

  // Git commit completion message
  TAsyncGitCommitCompletedMsg = class(TAsyncResultMsg)
  public
    constructor Create(const AOperationId: string; ASuccess: Boolean; 
                     const AErrorMessage: string = '');
  end;

  // Base class for background operations
  TAsyncOperation = class
  private
    FThread: TAsyncOperationThread;
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
  TAsyncOperationThread = class(TThread)
  private
    FOperation: TAsyncOperation;
    FOperationId: string;
  protected
    procedure Execute; override;
    procedure PostResultToMainThread(AResultMsg: bobaui.TMsg);
  public
    constructor Create(AOperation: TAsyncOperation; const AOperationId: string);
  end;

  // Specific async operation for commit message generation
  TAsyncCommitGeneration = class(TAsyncOperation)
  private
    FDiffContent: string;
    FCustomPrompt: string;
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ADiffContent: string; const ACustomPrompt: string);
  end;

// Global variable for thread communication - reference to the BobaUI program
var
  GProgram: TBobaUIProgram = nil;

// Forward declarations
{$IFDEF GITPAL_DEBUG}
procedure LogToFile(const LogMessage: string; const FileName: string = 'gitpal-debug.log'); forward;
{$ENDIF}
function ExecuteGitCommit(const CommitMessage: string; out ErrorMessage: string): Boolean; forward;
function GenerateCommitMessage(const DiffContent: string; const CustomPrompt: string = ''): string; forward;
function GenerateCommitMessageCmd(const DiffContent: string; const CustomPrompt: string): bobaui.TCmd; forward;

constructor TCommitGeneratedMsg.Create(const ACommitMessage: string);
begin
  inherited Create;
  CommitMessage := ACommitMessage;
end;

// TAsyncResultMsg implementations
constructor TAsyncResultMsg.Create(const AOperationId: string; ASuccess: Boolean; 
                                 const AErrorMessage: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FSuccess := ASuccess;
  FErrorMessage := AErrorMessage;
end;

// TAsyncCommitGeneratedMsg implementations
constructor TAsyncCommitGeneratedMsg.Create(const AOperationId: string; const ACommitMessage: string);
begin
  inherited Create(AOperationId, True, '');
  FCommitMessage := ACommitMessage;
end;

constructor TAsyncCommitGeneratedMsg.CreateError(const AOperationId: string; const AErrorMessage: string);
begin
  inherited Create(AOperationId, False, AErrorMessage);
  FCommitMessage := '';
end;

// TAsyncGitCommitCompletedMsg implementations
constructor TAsyncGitCommitCompletedMsg.Create(const AOperationId: string; ASuccess: Boolean; 
                                             const AErrorMessage: string);
begin
  inherited Create(AOperationId, ASuccess, AErrorMessage);
end;

// TAsyncOperation implementations
constructor TAsyncOperation.Create(const AOperationId: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FIsRunning := False;
  FThread := nil;
  FCS := TCriticalSection.Create;
end;

destructor TAsyncOperation.Destroy;
begin
  Cancel;
  FCS.Free;
  inherited Destroy;
end;

procedure TAsyncOperation.Start;
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncOperation.Start: Starting operation ' + FOperationId);
  {$ENDIF}
  FCS.Enter;
  try
    if not FIsRunning then
    begin
      FIsRunning := True;
      FThread := TAsyncOperationThread.Create(Self, FOperationId);
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperation.Start: Created thread for operation ' + FOperationId);
      {$ENDIF}
      FThread.Start;
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperation.Start: Started thread for operation ' + FOperationId);
      {$ENDIF}
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TAsyncOperation.Cancel;
begin
  FCS.Enter;
  try
    if FIsRunning and Assigned(FThread) then
    begin
      FThread.Terminate;
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;
      FIsRunning := False;
    end;
  finally
    FCS.Leave;
  end;
end;

function TAsyncOperation.IsRunning: Boolean;
begin
  FCS.Enter;
  try
    Result := FIsRunning;
  finally
    FCS.Leave;
  end;
end;

// Note: TAsyncMessageQueue class removed - now using BobaUI's built-in Program.Send() method

// TAsyncCommitGeneration implementations
constructor TAsyncCommitGeneration.Create(const AOperationId: string; const ADiffContent: string; const ACustomPrompt: string);
begin
  inherited Create(AOperationId);
  FDiffContent := ADiffContent;
  FCustomPrompt := ACustomPrompt;
end;

function TAsyncCommitGeneration.ExecuteOperation: bobaui.TMsg;
var
  CommitMessage: string;
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncCommitGeneration.ExecuteOperation: Starting commit generation for operation ' + FOperationId);
  {$ENDIF}
  try
    CommitMessage := GenerateCommitMessage(FDiffContent, FCustomPrompt);
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TAsyncCommitGeneration.ExecuteOperation: Got commit message: ' + Copy(CommitMessage, 1, 100) + '...');
    {$ENDIF}
    if Pos('Error:', CommitMessage) = 1 then
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncCommitGeneration.ExecuteOperation: Error in commit message: ' + CommitMessage);
      {$ENDIF}
      Result := TAsyncCommitGeneratedMsg.CreateError(FOperationId, CommitMessage);
    end
    else
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncCommitGeneration.ExecuteOperation: Success, creating result message');
      {$ENDIF}
      Result := TAsyncCommitGeneratedMsg.Create(FOperationId, CommitMessage);
    end;
  except
    on E: Exception do
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncCommitGeneration.ExecuteOperation: Exception: ' + E.Message);
      {$ENDIF}
      Result := TAsyncCommitGeneratedMsg.CreateError(FOperationId, 'Exception: ' + E.Message);
    end;
  end;
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncCommitGeneration.ExecuteOperation: Completed operation ' + FOperationId);
  {$ENDIF}
end;

// TAsyncOperationThread implementations
constructor TAsyncOperationThread.Create(AOperation: TAsyncOperation; const AOperationId: string);
begin
  inherited Create(False); // Create suspended
  FOperation := AOperation;
  FOperationId := AOperationId;
  FreeOnTerminate := False; // We'll manage the lifetime manually
end;

procedure TAsyncOperationThread.PostResultToMainThread(AResultMsg: bobaui.TMsg);
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncOperationThread.PostResultToMainThread: Posting result for operation ' + FOperationId);
  {$ENDIF}
  // Use BobaUI's new Send() method for thread-safe message sending
  if Assigned(GProgram) and Assigned(AResultMsg) then
  begin
    GProgram.Send(AResultMsg);
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TAsyncOperationThread.PostResultToMainThread: Successfully sent message via Program.Send()');
    {$ENDIF}
  end
  else
  begin
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TAsyncOperationThread.PostResultToMainThread: ERROR - GProgram or AResultMsg is nil');
    {$ENDIF}
  end;
end;

procedure TAsyncOperationThread.Execute;
var
  ResultMsg: bobaui.TMsg;
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncOperationThread.Execute: Starting thread execution for operation ' + FOperationId);
  {$ENDIF}
  try
    if not Terminated then
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperationThread.Execute: Calling ExecuteOperation for ' + FOperationId);
      {$ENDIF}
      // Execute the actual operation
      ResultMsg := FOperation.ExecuteOperation;
      
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperationThread.Execute: ExecuteOperation completed for ' + FOperationId);
      {$ENDIF}
      
      // Post result back to main thread
      if not Terminated and Assigned(ResultMsg) then
      begin
        {$IFDEF GITPAL_DEBUG}
        LogToFile('TAsyncOperationThread.Execute: Posting result to main thread for ' + FOperationId);
        {$ENDIF}
        PostResultToMainThread(ResultMsg);
      end
      else
      begin
        {$IFDEF GITPAL_DEBUG}
        LogToFile('TAsyncOperationThread.Execute: Cannot post result - Terminated=' + BoolToStr(Terminated) + ', ResultMsg assigned=' + BoolToStr(Assigned(ResultMsg)));
        {$ENDIF}
      end;
    end
    else
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperationThread.Execute: Thread was terminated before execution for ' + FOperationId);
      {$ENDIF}
    end;
  except
    on E: Exception do
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TAsyncOperationThread.Execute: Exception in thread: ' + E.Message);
      {$ENDIF}
      // Create error message
      ResultMsg := TAsyncCommitGeneratedMsg.CreateError(FOperationId, 'Thread exception: ' + E.Message);
      PostResultToMainThread(ResultMsg);
    end;
  end;
  
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncOperationThread.Execute: Marking operation as no longer running for ' + FOperationId);
  {$ENDIF}
  
  // Mark operation as no longer running
  FOperation.FCS.Enter;
  try
    FOperation.FIsRunning := False;
  finally
    FOperation.FCS.Leave;
  end;
  
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TAsyncOperationThread.Execute: Thread execution completed for ' + FOperationId);
  {$ENDIF}
end;

constructor TCommitModel.Create;
begin
  inherited Create;
  FTerminalWidth := 0;  // Will be set by WindowSizeMsg
  FTerminalHeight := 0; // Will be set by WindowSizeMsg
  FCommitMessage := AnsiString('');
  FCommitExecuted := False;
  FCommitSuccessful := False;
  FCommitErrorMessage := AnsiString('');
  FGenerating := False;
  FDiffContent := AnsiString('');
  FCustomPrompt := AnsiString('');
  FGenerationStarted := False;
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
  FList.SelectedColor := bobastyle.cBrightBlue;
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;

constructor TCommitModel.Create(const ACommitMessage: string);
begin
  inherited Create;
  FTerminalWidth := 0;  // Will be set by WindowSizeMsg
  FTerminalHeight := 0; // Will be set by WindowSizeMsg
  FCommitMessage := ACommitMessage;
  FCommitExecuted := False;
  FCommitSuccessful := False;
  FCommitErrorMessage := AnsiString('');
  FGenerating := False;
  FDiffContent := AnsiString('');
  FCustomPrompt := AnsiString('');
  FGenerationStarted := False;
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
  FList.SelectedColor := bobastyle.cBrightBlue;
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;

constructor TCommitModel.Create(const ADiffContent: string; const ACustomPrompt: string);
begin
  inherited Create;
  FTerminalWidth := 0;  // Will be set by WindowSizeMsg
  FTerminalHeight := 0; // Will be set by WindowSizeMsg
  FCommitMessage := AnsiString('');
  FCommitExecuted := False;
  FCommitSuccessful := False;
  FCommitErrorMessage := AnsiString('');
  FGenerating := True;
  FDiffContent := ADiffContent;
  FCustomPrompt := ACustomPrompt;
  FGenerationStarted := False;
  // Initialize async fields - start in idle state, will transition to generating when operation starts
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
  FList.SelectedColor := bobastyle.cBrightBlue;
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;


destructor TCommitModel.Destroy;
begin
  // Cancel any running async operation
  if Assigned(FCurrentOperation) then
  begin
    FCurrentOperation.Cancel;
    FCurrentOperation.Free;
  end;
  FList.Free;
  FSpinner.Free;
  inherited Destroy;
end;


function TCommitModel.View: string;
var
  Style: bobastyle.TStyle;
  BorderedMessage: string;
  PaddedContent: string;
  Sections: array of string;
begin
  // Handle initial state gracefully when terminal size is unknown
  if FTerminalWidth <= 0 then
  begin
    Result := AnsiString('Detecting terminal size...');
    Exit;
  end;
  
  // If we're still generating the commit message, show spinner
  if FGenerating then
  begin
    Result := FSpinner.View + AnsiString(' Analyzing staged changes and generating commit message...');
    Exit;
  end;
  
  // Add padding to the content (space before and after)
  PaddedContent := AnsiString(' ') + FCommitMessage + AnsiString(' ');
  
  Style := bobastyle.TStyle.Create;
  try
    Style.BorderStyle := bobastyle.bsSingle;
    Style.BorderColor := bobastyle.cBrightGreen;
    Style.Width := FTerminalWidth;
    Style.Content := PaddedContent;
    BorderedMessage := Style.Render;
  finally
    Style.Free;
  end;
  
  // Build initial UI that's always present
  SetLength(Sections, 5);
  Sections[0] := AnsiString('Generated commit message:');
  Sections[1] := AnsiString(''); // Empty line above box
  Sections[2] := BorderedMessage;
  Sections[3] := AnsiString(''); // Empty line below box
  Sections[4] := AnsiString('Accept this commit message?') + #10 + #10 + FList.View;
  
  // If commit has been executed, extend the array to append the result
  if FCommitExecuted then
  begin
    SetLength(Sections, 7);
    Sections[5] := AnsiString(''); // Empty line before result
    if FCommitSuccessful then
      Sections[6] := bobastyle.ColorText(AnsiString('✓ Commit successful!'), bobastyle.cBrightGreen)
    else
      Sections[6] := bobastyle.ColorText(AnsiString('✗ Commit failed: ') + FCommitErrorMessage, bobastyle.cBrightRed);
  end;
  
  Result := bobastyle.JoinVertical(Sections);
end;

// TCommitModel helper methods
function TCommitModel.GenerateOperationId: string;
begin
  Result := 'op_' + IntToStr(GetTickCount64);
end;

procedure TCommitModel.StartCommitGeneration;
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('TCommitModel.StartCommitGeneration: Current state = ' + IntToStr(Ord(FAsyncState)));
  {$ENDIF}
  if FAsyncState = osIdle then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osGenerating;
    
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TCommitModel.StartCommitGeneration: Starting operation ' + FOperationId + ' with diff length ' + IntToStr(Length(FDiffContent)));
    {$ENDIF}
    
    // Create and start the async operation
    FCurrentOperation := TAsyncCommitGeneration.Create(FOperationId, FDiffContent, FCustomPrompt);
    FCurrentOperation.Start;
    
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TCommitModel.StartCommitGeneration: Operation started successfully');
    {$ENDIF}
  end
  else
  begin
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TCommitModel.StartCommitGeneration: Cannot start - already in state ' + IntToStr(Ord(FAsyncState)));
    {$ENDIF}
  end;
end;

procedure TCommitModel.CancelCurrentOperation;
begin
  if Assigned(FCurrentOperation) then
  begin
    FCurrentOperation.Cancel;
    FCurrentOperation.Free;
    FCurrentOperation := nil;
  end;
  FAsyncState := osIdle;
end;

function ExecuteGitCommit(const CommitMessage: string; out ErrorMessage: string): Boolean;
var
  GitProcess: TProcess;
  OutputStr: string;
  BytesRead: longint;
  Buffer: array[0..2047] of char;
begin
  Result := False;
  ErrorMessage := AnsiString('');
  GitProcess := TProcess.Create(nil);
  try
    GitProcess.Executable := AnsiString('git');
    GitProcess.Parameters.Add(AnsiString('commit'));
    GitProcess.Parameters.Add(AnsiString('-m'));
    GitProcess.Parameters.Add(CommitMessage);
    GitProcess.Options := [poUsePipes, poStderrToOutPut];
    GitProcess.Execute;
    
    OutputStr := AnsiString('');
    while GitProcess.Running do
    begin
      BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        OutputStr := OutputStr + Copy(Buffer, 1, BytesRead);
    end;
    
    // Read any remaining output
    repeat
      BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        OutputStr := OutputStr + Copy(Buffer, 1, BytesRead);
    until BytesRead <= 0;
    
    GitProcess.WaitOnExit;
    
    {$IFDEF GITPAL_DEBUG}
    LogToFile('Git commit exit code: ' + IntToStr(GitProcess.ExitStatus));
    LogToFile('Git commit output: ' + OutputStr);
    {$ENDIF}
    
    Result := GitProcess.ExitStatus = 0;
    
    if not Result then
      ErrorMessage := OutputStr;
      
  finally
    GitProcess.Free;
  end;
end;

function TCommitModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  ListSelectionMsg: bobacomponents.TListSelectionMsg;
  ComponentTickMsg: bobaui.TComponentTickMsg;
  CommitGeneratedMsg: TCommitGeneratedMsg;
  AsyncCommitMsg: TAsyncCommitGeneratedMsg;
  NewModel: TCommitModel;
  NewList: bobacomponents.TList;
  NewSpinner: bobacomponents.TSpinner;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  // Handle async messages using the new BobaUI pattern - these now come directly through the message system
  if Msg is TAsyncCommitGeneratedMsg then
  begin
    AsyncCommitMsg := TAsyncCommitGeneratedMsg(Msg);
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TCommitModel.Update: Got async commit message for operation ' + AsyncCommitMsg.OperationId + ', current operation = ' + FOperationId);
    {$ENDIF}
    if AsyncCommitMsg.OperationId = FOperationId then
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TCommitModel.Update: Operation IDs match, processing result. Success = ' + BoolToStr(AsyncCommitMsg.Success));
      {$ENDIF}
      // This is our operation result
      CancelCurrentOperation; // Clean up
      
      if AsyncCommitMsg.Success then
      begin
        {$IFDEF GITPAL_DEBUG}
        LogToFile('TCommitModel.Update: Success! Creating new model with commit message: ' + Copy(AsyncCommitMsg.CommitMessage, 1, 100) + '...');
        {$ENDIF}
        // Success - create new model with commit message
        NewModel := TCommitModel.Create(AsyncCommitMsg.CommitMessage);
        NewModel.FTerminalWidth := FTerminalWidth;
        NewModel.FTerminalHeight := FTerminalHeight;
        Result.Model := NewModel;
      end
      else
      begin
        {$IFDEF GITPAL_DEBUG}
        LogToFile('TCommitModel.Update: Error in async operation: ' + AsyncCommitMsg.ErrorMessage);
        {$ENDIF}
        // Error - show error and quit
        writeln('Error generating commit message: ' + AsyncCommitMsg.ErrorMessage);
        Result.Cmd := bobaui.QuitCmd;
      end;
    end
    else
    begin
      {$IFDEF GITPAL_DEBUG}
      LogToFile('TCommitModel.Update: Operation ID mismatch, ignoring message');
      {$ENDIF}
    end;
    Exit; // Don't process other messages this cycle
  end;

  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Handle 'q' to quit
    if (KeyMsg.Key = 'q') or (KeyMsg.Key = 'Q') then
    begin
      CancelCurrentOperation; // Cancel any running operation
      Result.Cmd := bobaui.QuitCmd;
    end
    // If commit was executed, any key should quit
    else if FCommitExecuted then
    begin
      Result.Cmd := bobaui.QuitCmd;
    end
    else if FAsyncState = osIdle then  // Only handle list navigation if not busy
    begin
      // Delegate keys to the list component
      NewList := FList.Update(Msg);
      if NewList <> FList then
      begin
        NewModel := TCommitModel.Create(FCommitMessage);
        NewModel.FTerminalWidth := FTerminalWidth;
        NewModel.FTerminalHeight := FTerminalHeight;
        NewModel.FList.Free;
        NewModel.FList := NewList;
        
        // Check if list has a pending selection
        if NewList.HasPendingSelection then
        begin
          Result.Cmd := bobacomponents.ListSelectionCmd(
            NewList.SelectedIndex, 
            NewList.SelectedItem, 
            NewList.ListId
          );
          NewList.ClearPendingSelection;
        end;
        
        Result.Model := NewModel;
      end;
    end;
  end
  else if Msg is bobacomponents.TListSelectionMsg then
  begin
    // Handle list selection message
    ListSelectionMsg := bobacomponents.TListSelectionMsg(Msg);
    
    // Handle selection
    if ListSelectionMsg.SelectedIndex = 0 then
    begin
      // Accept: Execute git commit and update state
      FCommitSuccessful := ExecuteGitCommit(FCommitMessage, FCommitErrorMessage);
      FCommitExecuted := True;
      // Don't quit immediately, let the view show the result first
    end
    else
    begin
      // Decline: Just exit
      Result.Cmd := bobaui.QuitCmd;
    end;
  end
  else if Msg is bobaui.TComponentTickMsg then
  begin
    if FGenerating then  // Show spinner while generating
    begin
      // Update spinner during async operation
      NewSpinner := FSpinner.Update(Msg);
      if NewSpinner <> FSpinner then
      begin
        NewModel := TCommitModel.Create(FDiffContent, FCustomPrompt);
        NewModel.FTerminalWidth := FTerminalWidth;
        NewModel.FTerminalHeight := FTerminalHeight;
        NewModel.FAsyncState := FAsyncState;
        NewModel.FCurrentOperation := FCurrentOperation;
        NewModel.FOperationId := FOperationId;
        // Transfer ownership of operation to new model
        FCurrentOperation := nil;
        NewModel.FSpinner.Free;
        NewModel.FSpinner := NewSpinner;
        Result.Model := NewModel;
        Result.Cmd := FSpinner.Tick; // Continue animation
      end;
    end;
  end
  else if Msg is TCommitGeneratedMsg then
  begin
    // Legacy message handling - shouldn't happen with new async system
    CommitGeneratedMsg := TCommitGeneratedMsg(Msg);
    NewModel := TCommitModel.Create(CommitGeneratedMsg.CommitMessage);
    NewModel.FTerminalWidth := FTerminalWidth;
    NewModel.FTerminalHeight := FTerminalHeight;
    Result.Model := NewModel;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    // Always update dimensions without recreating model
    FTerminalWidth := WindowMsg.Width;
    FTerminalHeight := WindowMsg.Height;
    
    {$IFDEF GITPAL_DEBUG}
    LogToFile('TCommitModel.Update: Window size message - width=' + IntToStr(FTerminalWidth) + ', height=' + IntToStr(FTerminalHeight) + ', AsyncState=' + IntToStr(Ord(FAsyncState)) + ', CurrentOperation assigned=' + BoolToStr(Assigned(FCurrentOperation)));
    {$ENDIF}
    
    // Hide cursor on first window size message (initialization)
    if not FCursorHidden then
    begin
      FCursorHidden := True;
      
      // If we're waiting to generate and just got window size, start the generation
      if (FAsyncState = osIdle) and (FGenerating) and (not Assigned(FCurrentOperation)) then
      begin
        {$IFDEF GITPAL_DEBUG}
        LogToFile('TCommitModel.Update: Starting async commit generation from window size message');
        {$ENDIF}
        // Start spinner animation and async commit generation
        StartCommitGeneration;
        // Execute multiple commands: hide cursor and start ticker
        Result.Cmd := bobaui.BatchCmd([bobaui.HideCursorCmd, FSpinner.Tick]);
      end
      else
      begin
        // Just hide cursor
        Result.Cmd := bobaui.HideCursorCmd;
      end;
    end;
  end;
end;

procedure ShowHelp;
begin
  writeln('gitpal - AI-powered git assistant');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal [command]');
  writeln('');
  writeln('Available Commands:');
  writeln('  commit       Generate and apply AI-powered commit messages');
  writeln('  changelog    Update CHANGELOG.md with recent changes');
  writeln('');
  writeln('Options:');
  writeln('  --help, -h     Show this help message');
  writeln('  --version, -v  Show version information');
  writeln('');
  writeln('Use "gitpal [command] --help" for more information about a command.');
end;

procedure ShowCommitHelp;
begin
  writeln('gitpal commit - Generate AI-powered commit messages');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal commit [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes your git changes and generates a descriptive commit message');
  writeln('  using AI. You can review and accept or decline the suggestion.');
  writeln('');
  writeln('Options:');
  writeln('  --prompt <text>  Add custom instructions to the AI prompt');
  writeln('  --help, -h       Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal commit');
  writeln('  gitpal commit --prompt "Focus on performance improvements"');
  writeln('  gitpal commit --prompt "Don''t mention specific technology names"');
end;

procedure ShowChangelogHelp;
begin
  writeln('gitpal changelog - Update CHANGELOG.md file');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal changelog [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes recent commits and updates your CHANGELOG.md file with');
  writeln('  a summary of changes organized by type (features, fixes, etc.).');
  writeln('');
  writeln('Options:');
  writeln('  --help, -h   Show this help message');
end;

{$IFDEF GITPAL_DEBUG}
procedure LogToFile(const LogMessage: string; const FileName: string = 'gitpal-debug.log');
var
  LogFile: TextFile;
  Timestamp: string;
begin
  try
    AssignFile(LogFile, FileName);
    if FileExists(FileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    
    Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    WriteLn(LogFile, '[' + Timestamp + '] ' + LogMessage);
    CloseFile(LogFile);
  except
    // Silently ignore logging errors to avoid breaking the main functionality
  end;
end;
{$ENDIF}

function GetGitDiff: string;
var
  GitProcess: TProcess;
  OutputStr: string;
  BytesRead: longint;
  Buffer: array[0..2047] of char;
begin
  Result := AnsiString('');
  GitProcess := TProcess.Create(nil);
  try
    GitProcess.Executable := AnsiString('git');
    GitProcess.Parameters.Add(AnsiString('diff'));
    GitProcess.Parameters.Add(AnsiString('--cached'));
    GitProcess.Options := [poUsePipes, poStderrToOutPut];
    GitProcess.Execute;
    
    OutputStr := AnsiString('');
    while GitProcess.Running do
    begin
      BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        OutputStr := OutputStr + Copy(AnsiString(Buffer), 1, BytesRead);
    end;
    
    // Read any remaining output
    repeat
      BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        OutputStr := OutputStr + Copy(AnsiString(Buffer), 1, BytesRead);
    until BytesRead <= 0;
    
    GitProcess.WaitOnExit;
    Result := OutputStr;
  finally
    GitProcess.Free;
  end;
end;

function GenerateCommitMessage(const DiffContent: string; const CustomPrompt: string = ''): string;
var
  GeminiProvider: models.TGeminiProvider;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  ApiKey: string;
  SystemPrompt: string;
  UserPrompt: string;
begin
  Result := AnsiString('');
  
  // Get API key from environment
  ApiKey := models.TGeminiProvider.GetApiKeyFromEnvironment;
  if ApiKey = '' then
  begin
    Result := AnsiString('Error: GEMINI_API_KEY environment variable not set');
    Exit;
  end;
  
  // Create provider instance
  GeminiProvider := models.TGeminiProvider.Create(ApiKey);
  try
    // Set up messages
    SetLength(Messages, 2);
    
    SystemPrompt := AnsiString('You are an expert Git user and software developer. ') +
                   AnsiString('Your task is to analyze git diffs and create detailed, well-structured commit messages. ') +
                   AnsiString('Follow these conventions:') + #10 +
                   AnsiString('- First line: Short title under 50 characters using imperative mood') + #10 +
                   AnsiString('- Second line: Leave blank') + #10 +
                   AnsiString('- Following lines: Detailed description explaining WHAT changed and WHY') + #10 +
                   AnsiString('- Use conventional commit prefixes when appropriate (feat:, fix:, docs:, refactor:, etc.)') + #10 +
                   AnsiString('- Include 1-2 paragraphs describing the changes in detail') + #10 +
                   AnsiString('- Add blank lines between paragraphs for better readability') + #10 +
                   AnsiString('- Explain the motivation and context behind the changes') + #10 +
                   AnsiString('- Mention any important implementation details or considerations') + #10 +
                   AnsiString('- Use imperative mood throughout (e.g., "Add feature" not "Added feature")') + #10 +
                   AnsiString('Return ONLY the commit message, no explanations or additional formatting.');
    
    // Add custom prompt if provided
    if CustomPrompt <> '' then
      SystemPrompt := SystemPrompt + #10 + #10 + AnsiString('Additional instructions: ') + CustomPrompt;
    
    UserPrompt := AnsiString('Please analyze this git diff and generate an appropriate commit message:') + #10 + #10 + DiffContent;
    
    Messages[0].Role := models.lmrSystem;
    Messages[0].Content := SystemPrompt;
    Messages[1].Role := models.lmrUser;
    Messages[1].Content := UserPrompt;
    
    // Log the request for debugging
    {$IFDEF GITPAL_DEBUG}
    LogToFile('=== AI REQUEST ===');
    LogToFile('System Prompt: ' + SystemPrompt);
    LogToFile('User Prompt: ' + UserPrompt);
    LogToFile('==================');
    {$ENDIF}
    
    // Call ChatCompletion
    Response := GeminiProvider.ChatCompletion(
      GeminiProvider.GetDefaultModel,
      Messages,
      0.3,    // Lower temperature for more consistent results
      400     // Max tokens for detailed commit message with description
    );
    
    // Extract the response
    if Length(Response.Choices) > 0 then
    begin
      Result := Response.Choices[0].Message.Content;
      // Log the response for debugging
      {$IFDEF GITPAL_DEBUG}
      LogToFile('=== AI RESPONSE ===');
      LogToFile('Raw Response: ' + Result);
      LogToFile('===================');
      {$ENDIF}
    end
    else
    begin
      Result := AnsiString('Error: No response from LLM');
      {$IFDEF GITPAL_DEBUG}
      LogToFile('ERROR: No response from LLM');
      {$ENDIF}
    end;
      
  finally
    GeminiProvider.Free;
  end;
end;

// Command class for generating commit message asynchronously
type
  TGenerateCommitMessageCommand = class(bobaui.TCommand)
  private
    FDiffContent: string;
    FCustomPrompt: string;
  public
    constructor Create(const ADiffContent: string; const ACustomPrompt: string);
    function Execute: bobaui.TMsg; override;
  end;

constructor TGenerateCommitMessageCommand.Create(const ADiffContent: string; const ACustomPrompt: string);
begin
  inherited Create;
  FDiffContent := ADiffContent;
  FCustomPrompt := ACustomPrompt;
end;

function TGenerateCommitMessageCommand.Execute: bobaui.TMsg;
var
  CommitMessage: string;
begin
  CommitMessage := GenerateCommitMessage(FDiffContent, FCustomPrompt);
  Result := TCommitGeneratedMsg.Create(CommitMessage);
end;

// Helper function to create the command
function GenerateCommitMessageCmd(const DiffContent: string; const CustomPrompt: string): bobaui.TCmd;
begin
  Result := TGenerateCommitMessageCommand.Create(DiffContent, CustomPrompt);
end;

procedure RunCommitCommand(const CustomPrompt: string = '');
var
  Prog: TBobaUIProgram;
  Model: TCommitModel;
  DiffContent: string;
begin
  {$IFDEF GITPAL_DEBUG}
  LogToFile('RunCommitCommand: Starting commit command with custom prompt: ' + CustomPrompt);
  {$ENDIF}
  
  // Check if there are staged changes
  DiffContent := GetGitDiff;
  if DiffContent = '' then
  begin
    writeln('No staged changes found. Use "git add" to stage changes before generating a commit message.');
    Exit;
  end;
  
  {$IFDEF GITPAL_DEBUG}
  LogToFile('RunCommitCommand: Got git diff with length: ' + IntToStr(Length(DiffContent)));
  {$ENDIF}
  
  // Note: No need to initialize async message queue - using BobaUI's built-in Send() method
  
  // Create model that will show spinner and generate message async
  Model := TCommitModel.Create(DiffContent, CustomPrompt);
  {$IFDEF GITPAL_DEBUG}
  LogToFile('RunCommitCommand: Created TCommitModel with async state');
  {$ENDIF}
  
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set the global program reference for async operations
  GProgram := Prog;
  
  try
    {$IFDEF GITPAL_DEBUG}
    LogToFile('RunCommitCommand: Starting bobaui program');
    {$ENDIF}
    Prog.Run;
    {$IFDEF GITPAL_DEBUG}
    LogToFile('RunCommitCommand: Bobaui program finished');
    {$ENDIF}
  finally
    GProgram := nil;
    Prog.Free;
  end;
  {$IFDEF GITPAL_DEBUG}
  LogToFile('RunCommitCommand: Command completed');
  {$ENDIF}
end;

procedure RunChangelogCommand;
begin
  writeln('Changelog functionality coming soon...');
end;

var
  Command: string;
  CustomPrompt: string;
  i: integer;
  ShowMainHelp: boolean;
begin
  ShowMainHelp := false;
  Command := AnsiString('');
  CustomPrompt := AnsiString('');
  
  // Parse command line arguments
  if ParamCount = 0 then
    ShowMainHelp := true
  else
  begin
    i := 1;
    while i <= ParamCount do
    begin
      if (ParamStr(i) = '--help') or (ParamStr(i) = '-h') then
      begin
        if Command = AnsiString('') then
          ShowMainHelp := true
        else if Command = AnsiString('commit') then
        begin
          ShowCommitHelp;
          Exit;
        end
        else if Command = AnsiString('changelog') then
        begin
          ShowChangelogHelp;
          Exit;
        end;
      end
      else if (ParamStr(i) = '--version') or (ParamStr(i) = '-v') then
      begin
        writeln(AppVersion);
        Exit;
      end
      else if ParamStr(i) = '--prompt' then
      begin
        // Get the next parameter as the prompt value
        if i < ParamCount then
        begin
          Inc(i);
          CustomPrompt := AnsiString(ParamStr(i));
        end
        else
        begin
          writeln('Error: --prompt flag requires a value');
          Halt(1);
        end;
      end
      else if (Command = AnsiString('')) and (ParamStr(i)[1] <> '-') then
        Command := AnsiString(ParamStr(i));
      
      Inc(i);
    end;
  end;
  
  // Show help or execute command
  if ShowMainHelp then
  begin
    ShowHelp;
    Exit;
  end;
  
  if Command = AnsiString('commit') then
    RunCommitCommand(CustomPrompt)
  else if Command = AnsiString('changelog') then
    RunChangelogCommand
  else
  begin
    writeln('Error: Unknown command "', Command, '"');
    writeln('');
    ShowHelp;
    Halt(1);
  end;
end.
