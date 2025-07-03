unit command_commit;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  bobaui,
  bobastyle,
  bobacomponents,
  models,
  config_manager,
  provider_config,
  git,
  logging,
  SysUtils,
  Process,
  Classes,
  SyncObjs;

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
    FProviderOverride: string;
    FGenerationStarted: Boolean;
    // Async operation fields
    FAsyncState: TAsyncOperationState;
    FCurrentOperation: TAsyncOperation;
    FOperationId: string;
    FCursorHidden: Boolean;
    // Streaming fields
    FStreamingText: string;
    FIsStreaming: Boolean;
    
    procedure StartCommitGeneration;
    procedure CancelCurrentOperation;
    function GenerateOperationId: string;
  public
    constructor Create; overload;
    constructor Create(const ACommitMessage: string); overload;
    constructor Create(const ADiffContent: string; const ACustomPrompt: string); overload;
    constructor Create(const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string); overload;
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

  // Streaming chunk message for real-time updates
  TAsyncStreamingChunkMsg = class(TAsyncResultMsg)
  private
    FChunk: string;
    FIsComplete: Boolean;
  public
    constructor Create(const AOperationId: string; const AChunk: string; AIsComplete: Boolean);
    property Chunk: string read FChunk;
    property IsComplete: Boolean read FIsComplete;
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
    FProviderOverride: string;
    FCommitMessage: string;
    FStreamingFinished: Boolean;
    FStreamingError: string;
    procedure StreamCallback(const AChunk: string; AFinished: Boolean);
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string = '');
  end;

// Command class for generating commit message asynchronously
type
  TGenerateCommitMessageCommand = class(bobaui.TCommand)
  private
    FDiffContent: string;
    FCustomPrompt: string;
    FProviderOverride: string;
  public
    constructor Create(const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string = '');
    function Execute: bobaui.TMsg; override;
  end;

// Public functions
procedure RunCommitCommand(const CustomPrompt: string = ''; const StageChanges: Boolean = False; const ProviderOverride: string = '');


implementation

// Global variable for thread communication - reference to the BobaUI program
var
  GProgram: TBobaUIProgram = nil;

// Forward declarations
function GenerateCommitMessage(const DiffContent: string; const CustomPrompt: string = ''; const ProviderOverride: string = ''): string; forward;
procedure GenerateCommitMessageStream(const DiffContent: string; const CustomPrompt: string; ACallback: models.TLLMStreamCallback; const ProviderOverride: string = ''); forward;
function GenerateCommitMessageCmd(const DiffContent: string; const CustomPrompt: string; const ProviderOverride: string = ''): bobaui.TCmd; forward;

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

// TAsyncStreamingChunkMsg implementations
constructor TAsyncStreamingChunkMsg.Create(const AOperationId: string; const AChunk: string; AIsComplete: Boolean);
begin
  inherited Create(AOperationId, True, ''); // Streaming chunks are always "successful"
  FChunk := AChunk;
  FIsComplete := AIsComplete;
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
  try
    Cancel;
    FCS.Free;
    inherited Destroy;
  except
    // Silently ignore cleanup exceptions
  end;
end;

procedure TAsyncOperation.Start;
begin
  FCS.Enter;
  try
    if not FIsRunning then
    begin
      FIsRunning := True;
      FThread := TAsyncOperationThread.Create(Self, FOperationId);
      FThread.Start;
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
      Sleep(10); // Small delay to ensure thread cleanup is complete
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
constructor TAsyncCommitGeneration.Create(const AOperationId: string; const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string);
begin
  inherited Create(AOperationId);
  FDiffContent := ADiffContent;
  FCustomPrompt := ACustomPrompt;
  FProviderOverride := AProviderOverride;
  FCommitMessage := AnsiString('');
  FStreamingFinished := False;
  FStreamingError := AnsiString('');
end;

procedure TAsyncCommitGeneration.StreamCallback(const AChunk: string; AFinished: Boolean);
var
  StreamMsg: TAsyncStreamingChunkMsg;
begin
  DebugLog('[StreamCallback] Called - Finished: ' + BoolToStr(AFinished, True) + ', Chunk length: ' + IntToStr(Length(AChunk)));
  
  if AFinished then
  begin
    FStreamingFinished := True;
    DebugLog('[StreamCallback] Streaming finished, total message length: ' + IntToStr(Length(FCommitMessage)));
    if Pos('Error:', FCommitMessage) = 1 then
      FStreamingError := FCommitMessage;
  end
  else
  begin
    FCommitMessage := FCommitMessage + AChunk;
    DebugLog('[StreamCallback] Current message length: ' + IntToStr(Length(FCommitMessage)));
  end;
  
  // Send real-time streaming chunk to UI
  if Assigned(GProgram) then
  begin
    StreamMsg := TAsyncStreamingChunkMsg.Create(FOperationId, AChunk, AFinished);
    try
      GProgram.Send(StreamMsg);
    except
      on E: Exception do
      begin
        StreamMsg.Free;
      end;
    end;
  end;
end;

function TAsyncCommitGeneration.ExecuteOperation: bobaui.TMsg;
begin
  try
    DebugLog('[TAsyncCommitGeneration.ExecuteOperation] Starting async generation');
    DebugLog('[TAsyncCommitGeneration.ExecuteOperation] DiffContent length: ' + IntToStr(Length(FDiffContent)));
    
    GenerateCommitMessageStream(FDiffContent, FCustomPrompt, @StreamCallback, FProviderOverride);
    
    // Wait for streaming to complete
    while not FStreamingFinished do
    begin
      Sleep(10); // Small delay to avoid busy waiting
    end;
    
    DebugLog('[TAsyncCommitGeneration.ExecuteOperation] Streaming finished');
    
    if FStreamingError <> '' then
    begin
      Result := TAsyncCommitGeneratedMsg.CreateError(FOperationId, FStreamingError);
    end
    else
    begin
      Result := TAsyncCommitGeneratedMsg.Create(FOperationId, FCommitMessage);
    end;
  except
    on E: Exception do
    begin
      Result := TAsyncCommitGeneratedMsg.CreateError(FOperationId, 'Exception: ' + E.Message);
    end;
  end;
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
  try
    // Use BobaUI's new Send() method for thread-safe message sending
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

procedure TAsyncOperationThread.Execute;
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
      end;
    end
    else
    begin
    end;
  except
    on E: Exception do
    begin
      // Create error message
      ResultMsg := TAsyncCommitGeneratedMsg.CreateError(FOperationId, 'Thread exception: ' + E.Message);
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
  FProviderOverride := AnsiString('');
  FGenerationStarted := False;
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  // Initialize streaming fields
  FStreamingText := AnsiString('');
  FIsStreaming := False;
  
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
  FProviderOverride := AnsiString('');
  FGenerationStarted := False;
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  // Initialize streaming fields
  FStreamingText := AnsiString('');
  FIsStreaming := False;
  
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
  FProviderOverride := AnsiString('');
  FGenerationStarted := False;
  // Initialize async fields - start in idle state, will transition to generating when operation starts
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  // Initialize streaming fields
  FStreamingText := AnsiString('');
  FIsStreaming := False;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
  FList.SelectedColor := bobastyle.cBrightBlue;
  
  FSpinner := bobacomponents.TSpinner.Create(bobacomponents.stDot);
end;

constructor TCommitModel.Create(const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string);
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
  FProviderOverride := AProviderOverride;
  FGenerationStarted := False;
  // Initialize async fields - start in idle state, will transition to generating when operation starts
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := AnsiString('');
  FCursorHidden := False;
  // Initialize streaming fields
  FStreamingText := AnsiString('');
  FIsStreaming := False;
  
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
  
  // If we're still generating the commit message, show spinner or streaming text
  if FGenerating then
  begin
    if FIsStreaming and (FStreamingText <> '') then
    begin
      // Show streaming text in real-time
      PaddedContent := AnsiString(' ') + FStreamingText + AnsiString(' ');
      Style := bobastyle.TStyle.Create;
      try
        Style.BorderStyle := bobastyle.bsSingle;
        Style.BorderColor := bobastyle.cBrightYellow; // Different color to indicate streaming
        Style.Width := FTerminalWidth;
        Style.Content := PaddedContent;
        BorderedMessage := Style.Render;
      finally
        Style.Free;
      end;
      Result := AnsiString('Generating commit message (streaming)...') + #10 + #10 + BorderedMessage;
    end
    else
    begin
      // Show spinner while waiting for first chunk
      Result := FSpinner.View + AnsiString(' Analyzing staged changes and generating commit message...');
    end;
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
  if FAsyncState = osIdle then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osGenerating;
    
    
    // Create and start the async operation
    FCurrentOperation := TAsyncCommitGeneration.Create(FOperationId, FDiffContent, FCustomPrompt, FProviderOverride);
    FCurrentOperation.Start;
    
  end
  else
  begin
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

function TCommitModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  ListSelectionMsg: bobacomponents.TListSelectionMsg;
  CommitGeneratedMsg: TCommitGeneratedMsg;
  AsyncCommitMsg: TAsyncCommitGeneratedMsg;
  StreamChunkMsg: TAsyncStreamingChunkMsg;
  NewModel: TCommitModel;
  NewList: bobacomponents.TList;
  NewSpinner: bobacomponents.TSpinner;
  GitResult: git.TGitResult;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  // Handle async messages using the new BobaUI pattern - these now come directly through the message system
  if Msg is TAsyncCommitGeneratedMsg then
  begin
    AsyncCommitMsg := TAsyncCommitGeneratedMsg(Msg);
    if AsyncCommitMsg.OperationId = FOperationId then
    begin
      // This is our operation result
      CancelCurrentOperation; // Clean up
      
      if AsyncCommitMsg.Success then
      begin
        // Success - create new model with commit message
        NewModel := TCommitModel.Create(AsyncCommitMsg.CommitMessage);
        NewModel.FTerminalWidth := FTerminalWidth;
        NewModel.FTerminalHeight := FTerminalHeight;
        Result.Model := NewModel;
      end
      else
      begin
        // Error - show error and quit
        writeln('Error generating commit message: ' + AsyncCommitMsg.ErrorMessage);
        Result.Cmd := bobaui.QuitCmd;
      end;
    end
    else
    begin
    end;
    Exit; // Don't process other messages this cycle
  end;

  // Handle streaming chunk messages for real-time updates
  if Msg is TAsyncStreamingChunkMsg then
  begin
    StreamChunkMsg := TAsyncStreamingChunkMsg(Msg);
    if StreamChunkMsg.OperationId = FOperationId then
    begin      
      if not StreamChunkMsg.IsComplete then
      begin
        // Add chunk to streaming text
        FStreamingText := FStreamingText + StreamChunkMsg.Chunk;
        FIsStreaming := True;
      end
      else
      begin
        // Streaming is complete, but keep showing the streaming text
        // The final TAsyncCommitGeneratedMsg will handle the transition to the final state
        FIsStreaming := False;
      end;
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
      GitResult := git.TGitRepository.Commit(FCommitMessage);
      FCommitSuccessful := GitResult.Success;
      if not GitResult.Success then
        FCommitErrorMessage := GitResult.ErrorMessage
      else
        FCommitErrorMessage := AnsiString('');
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
      // Update spinner in place during async operation instead of creating new model
      NewSpinner := FSpinner.Update(Msg);
      if NewSpinner <> FSpinner then
      begin
        FSpinner.Free;
        FSpinner := NewSpinner;
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
    // Update dimensions in place without recreating model
    FTerminalWidth := WindowMsg.Width;
    FTerminalHeight := WindowMsg.Height;
    
    // Hide cursor on first window size message (initialization)
    if not FCursorHidden then
    begin
      FCursorHidden := True;
      
      // If we're waiting to generate and just got window size, start the generation
      if (FAsyncState = osIdle) and (FGenerating) and (not Assigned(FCurrentOperation)) then
      begin
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






function GenerateCommitMessage(const DiffContent: string; const CustomPrompt: string = ''; const ProviderOverride: string = ''): string;
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  Messages: array of models.TLLMMessage;
  Response: models.TLLMChatCompletionResponse;
  SystemPrompt: string;
  UserPrompt: string;
begin
  Result := AnsiString('');
  DebugLog('[GenerateCommitMessage] Starting commit message generation');
  
  try
    // Load configuration
    Config := TGitPalConfig.Create;
    Registry := TProviderRegistry.Create;
    try
      if not Config.LoadConfig then
      begin
        DebugLog('[GenerateCommitMessage] No configuration found');
        Result := AnsiString('Error: No configuration found. Please run "gitpal setup" first.');
        Exit;
      end;
      
      // Determine which provider to use
      if ProviderOverride <> '' then
        ProviderName := AnsiString(ProviderOverride)
      else
        ProviderName := Config.DefaultProvider;
      
      if ProviderName = '' then
      begin
        DebugLog('[GenerateCommitMessage] No provider specified');
        Result := AnsiString('Error: No provider specified and no default provider configured.');
        Exit;
      end;
      
      DebugLog('[GenerateCommitMessage] Using provider: ' + string(ProviderName));
      
      // Create provider from config
      try
        Provider := Registry.CreateProviderFromConfig(Config, ProviderName);
      except
        on E: Exception do
        begin
          DebugLog('[GenerateCommitMessage] Error creating provider: ' + E.Message);
          Result := AnsiString('Error: ' + E.Message);
          Exit;
        end;
      end;
      
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
      
      try
        // Debug: Log before API call
        DebugLog('[DEBUG] Calling ChatCompletion with model: ' + Provider.GetDefaultModel);
        DebugLog('[DEBUG] Messages count: ' + IntToStr(Length(Messages)));
        DebugLog('[DEBUG] System prompt length: ' + IntToStr(Length(SystemPrompt)));
        DebugLog('[DEBUG] User prompt length: ' + IntToStr(Length(UserPrompt)));
        DebugLog('[DEBUG] About to call Provider.ChatCompletion');
        
        // Call ChatCompletion
        Response := Provider.ChatCompletion(
          Provider.GetDefaultModel,
          Messages,
          0.3,    // Lower temperature for more consistent results
          2000    // Max tokens: Allow for detailed commit message
        );
        
        DebugLog('[DEBUG] ChatCompletion call completed');
        
        // Debug: Log response details
        DebugLog('[DEBUG] Response received. Choices count: ' + IntToStr(Length(Response.Choices)));
        
        // Extract the response
        if Length(Response.Choices) > 0 then
        begin
          DebugLog('[DEBUG] First choice content length: ' + IntToStr(Length(Response.Choices[0].Message.Content)));
          DebugLog('[DEBUG] First choice content (first 100 chars): ' + Copy(Response.Choices[0].Message.Content, 1, 100));
          
          Result := Response.Choices[0].Message.Content;
          
          // Debug: Check if result is empty after assignment
          if Length(Result) = 0 then
          begin
            DebugLog('[DEBUG] WARNING: Result is empty after assignment!');
          end
          else
          begin
            DebugLog('[DEBUG] Result length after assignment: ' + IntToStr(Length(Result)));
          end;
        end
        else
        begin
          DebugLog('[DEBUG] ERROR: No choices in response!');
          Result := AnsiString('Error: No response from LLM');
        end;
      except
        on E: Exception do
        begin
          DebugLog('[DEBUG] EXCEPTION in ChatCompletion: ' + E.ClassName + ': ' + E.Message);
          Result := AnsiString('Error: ' + E.Message);
        end;
      end;
      
    finally
      Config.Free;
      Registry.Free;
    end;
  except
    on E: Exception do
    begin
      DebugLog('[GenerateCommitMessage] Top-level exception: ' + E.Message);
      Result := AnsiString('Error: ' + E.Message);
    end;
  end;
end;

procedure GenerateCommitMessageStream(const DiffContent: string; const CustomPrompt: string; ACallback: models.TLLMStreamCallback; const ProviderOverride: string);
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  Messages: array of models.TLLMMessage;
  SystemPrompt: string;
  UserPrompt: string;
begin
  DebugLog('[GenerateCommitMessageStream] Starting streaming generation');
  
  try
    // Load configuration
    Config := TGitPalConfig.Create;
    Registry := TProviderRegistry.Create;
    try
      if not Config.LoadConfig then
      begin
        DebugLog('[GenerateCommitMessageStream] No configuration found');
        ACallback('Error: No configuration found. Please run "gitpal setup" first.', True);
        Exit;
      end;
      
      // Determine which provider to use
      if ProviderOverride <> '' then
        ProviderName := AnsiString(ProviderOverride)
      else
        ProviderName := Config.DefaultProvider;
      
      if ProviderName = '' then
      begin
        DebugLog('[GenerateCommitMessageStream] No provider specified');
        ACallback('Error: No provider specified and no default provider configured.', True);
        Exit;
      end;
      
      DebugLog('[GenerateCommitMessageStream] Using provider: ' + string(ProviderName));
      
      // Create provider from config
      try
        Provider := Registry.CreateProviderFromConfig(Config, ProviderName);
      except
        on E: Exception do
        begin
          DebugLog('[GenerateCommitMessageStream] Error creating provider: ' + E.Message);
          ACallback('Error: ' + E.Message, True);
          Exit;
        end;
      end;
      
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
    
      try
        DebugLog('[GenerateCommitMessageStream] About to call ChatCompletionStream');
        DebugLog('[GenerateCommitMessageStream] Model: ' + Provider.GetDefaultModel);
        
        // Call ChatCompletionStream
        Provider.ChatCompletionStream(
          Provider.GetDefaultModel,
          Messages,
          ACallback,
          0.3,    // Lower temperature for more consistent results
          2000    // Max tokens: Allow for detailed commit message
        );
        
        DebugLog('[GenerateCommitMessageStream] ChatCompletionStream call completed');
      except
        on E: Exception do
        begin
          DebugLog('[GenerateCommitMessageStream] Exception: ' + E.Message);
          ACallback('Error: ' + E.Message, True);
        end;
      end;
        
    finally
      Config.Free;
      Registry.Free;
    end;
  except
    on E: Exception do
    begin
      DebugLog('[GenerateCommitMessageStream] Top-level exception: ' + E.Message);
      ACallback('Error: ' + E.Message, True);
    end;
  end;
end;

constructor TGenerateCommitMessageCommand.Create(const ADiffContent: string; const ACustomPrompt: string; const AProviderOverride: string);
begin
  inherited Create;
  FDiffContent := ADiffContent;
  FCustomPrompt := ACustomPrompt;
  FProviderOverride := AProviderOverride;
end;

function TGenerateCommitMessageCommand.Execute: bobaui.TMsg;
var
  CommitMessage: string;
begin
  CommitMessage := GenerateCommitMessage(FDiffContent, FCustomPrompt, FProviderOverride);
  Result := TCommitGeneratedMsg.Create(CommitMessage);
end;

// Helper function to create the command
function GenerateCommitMessageCmd(const DiffContent: string; const CustomPrompt: string; const ProviderOverride: string): bobaui.TCmd;
begin
  Result := TGenerateCommitMessageCommand.Create(DiffContent, CustomPrompt, ProviderOverride);
end;

procedure RunCommitCommand(const CustomPrompt: string = ''; const StageChanges: Boolean = False; const ProviderOverride: string = '');
var
  Prog: TBobaUIProgram;
  Model: TCommitModel;
  DiffContent: string;
  UnstagedResult: git.TGitResult;
  AddResult: git.TGitResult;
  DiffResult: git.TGitResult;
begin
  
  // Handle staging if requested
  if StageChanges then
  begin
    // Check if there are unstaged changes first
    UnstagedResult := git.TGitRepository.GetDiff(['--name-only']);
    if not UnstagedResult.Success or (Trim(UnstagedResult.Output) = '') then
    begin
      writeln('No unstaged changes found to stage.');
      // Continue to check for already staged changes
    end
    else
    begin
      writeln('Staging changes...');
      AddResult := git.TGitRepository.Add(['.']);
      if not AddResult.Success then
      begin
        writeln('Error staging changes: ', AddResult.ErrorMessage);
        Exit;
      end;
      writeln('Changes staged successfully.');
    end;
  end;
  
  // Check if there are staged changes
  DiffResult := git.TGitRepository.GetDiff(['--cached']);
  if not DiffResult.Success then
  begin
    writeln('Error getting git diff: ', DiffResult.ErrorMessage);
    Exit;
  end;
  DiffContent := DiffResult.Output;
  if DiffContent = '' then
  begin
    if StageChanges then
      writeln('No changes to commit after staging.')
    else
      writeln('No staged changes found. Use "git add" to stage changes or use --stage flag to stage all changes.');
    Exit;
  end;
  
  // Create model that will show spinner and generate message async
  Model := TCommitModel.Create(DiffContent, CustomPrompt, ProviderOverride);
  
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

// Testing
