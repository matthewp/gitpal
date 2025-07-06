unit command_changelog;

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
  SysUtils,
  Process,
  Classes,
  fpjson,
  jsonparser,
  SyncObjs;

// Forward declarations for async operations
type
  TAsyncOperation = class;
  TAsyncOperationThread = class;

// Async operation state enumeration
type
  TAsyncOperationState = (
    osIdle,           // No operation running
    osGenerating,     // Generating changelog
    osCancelling      // Cancelling current operation
  );

// Base class for async operation results
type
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

// Changelog generation result message
type
  TAsyncChangelogCompletedMsg = class(TAsyncResultMsg)
  private
    FResult: string;
  public
    constructor Create(const AOperationId: string; const AResult: string);
    constructor CreateError(const AOperationId: string; const AErrorMessage: string);
    property Result: string read FResult;
  end;

// Base class for background operations
type
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
type
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

// Specific async operation for changelog generation
type
  TAsyncChangelogGeneration = class(TAsyncOperation)
  private
    FRepoPath: string;
    FProviderOverride: string;
    FFromTag: string;
    FToTag: string;
  protected
    function ExecuteOperation: bobaui.TMsg; override;
  public
    constructor Create(const AOperationId: string; const ARepoPath: string = '.'; const AProviderOverride: string = ''; const AFromTag: string = ''; const AToTag: string = '');
  end;

// Changelog UI Model
type
  TChangelogModel = class(bobaui.TModel)
  private
    FSpinner: TSpinner;
    FState: string;
    FErrorMessage: string;
    FSuccessMessage: string;
    FIsGenerating: Boolean;
    FProviderOverride: string;
    FFromTag: string;
    FToTag: string;
    // Async operation fields
    FAsyncState: TAsyncOperationState;
    FCurrentOperation: TAsyncOperation;
    FOperationId: string;
    FTerminalWidth: Integer;
    FTerminalHeight: Integer;
    
    procedure StartChangelogGeneration;
    procedure CancelCurrentOperation;
    function GenerateOperationId: string;
  public
    constructor Create(const ProviderOverride: string = ''; const FromTag: string = ''; const ToTag: string = '');
    destructor Destroy; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
    function View: string; override;
    function Init: bobaui.TCmd;
    procedure StartGeneration;
    procedure SetSuccess(const Message: string);
    procedure SetError(const Message: string);
    property IsGenerating: Boolean read FIsGenerating;
  end;

// Tool context for changelog generation
type
  TGitPalToolContext = class(TBaseToolContext)
  private
    FRepoPath: string;
    FFromTag: string;
    FToTag: string;
    function ValidateFilePath(const Path: string): Boolean;
    
    // Tool implementations
    function GetGitTags: string;
    function GetCommitsBetween(const FromRef, ToRef: string): string;
    function ReadChangelogFile(const FilePath: string): string;
    function WriteChangelogFile(const Content, FilePath: string): Boolean;
  public
    constructor Create(const ARepoPath: string = '.'; const AFromTag: string = ''; const AToTag: string = '');
    destructor Destroy; override;
    function ExecuteTool(const ToolCall: TToolCall): TToolResult; override;
    function GetAvailableTools: TToolFunctionArray; override;
  end;

// Public functions for changelog command
procedure RunChangelogCommand(const ProviderOverride: string = ''; const FromTag: string = ''; const ToTag: string = '');

implementation

// Global variable for thread communication - reference to the BobaUI program
var
  GProgram: TBobaUIProgram = nil;

// Forward declaration for the original generation function
function GenerateChangelogAsync(const RepoPath: string; const ProviderOverride: string = ''; const FromTag: string = ''; const ToTag: string = ''): string; forward;

{ TAsyncResultMsg }

constructor TAsyncResultMsg.Create(const AOperationId: string; ASuccess: Boolean; 
                                 const AErrorMessage: string);
begin
  inherited Create;
  FOperationId := AOperationId;
  FSuccess := ASuccess;
  FErrorMessage := AErrorMessage;
end;

{ TAsyncChangelogCompletedMsg }

constructor TAsyncChangelogCompletedMsg.Create(const AOperationId: string; const AResult: string);
begin
  inherited Create(AOperationId, True, '');
  FResult := AResult;
end;

constructor TAsyncChangelogCompletedMsg.CreateError(const AOperationId: string; const AErrorMessage: string);
begin
  inherited Create(AOperationId, False, AErrorMessage);
  FResult := '';
end;

{ TAsyncOperation }

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

{ TAsyncOperationThread }

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
      else if Assigned(ResultMsg) then
      begin
        ResultMsg.Free;
      end;
    end
  except
    on E: Exception do
    begin
      // Create error message
      ResultMsg := TAsyncChangelogCompletedMsg.CreateError(FOperationId, 'Thread exception: ' + E.Message);
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

{ TAsyncChangelogGeneration }

constructor TAsyncChangelogGeneration.Create(const AOperationId: string; const ARepoPath: string; const AProviderOverride: string; const AFromTag: string; const AToTag: string);
begin
  inherited Create(AOperationId);
  FRepoPath := ARepoPath;
  FProviderOverride := AProviderOverride;
  FFromTag := AFromTag;
  FToTag := AToTag;
end;

function TAsyncChangelogGeneration.ExecuteOperation: bobaui.TMsg;
var
  GenerationResult: string;
begin
  try
    GenerationResult := GenerateChangelogAsync(FRepoPath, FProviderOverride, FFromTag, FToTag);
    Result := TAsyncChangelogCompletedMsg.Create(FOperationId, GenerationResult);
  except
    on E: Exception do
    begin
      Result := TAsyncChangelogCompletedMsg.CreateError(FOperationId, 'Exception: ' + E.Message);
    end;
  end;
end;

{ TChangelogModel }

constructor TChangelogModel.Create(const ProviderOverride: string; const FromTag: string; const ToTag: string);
begin
  inherited Create;
  FSpinner := TSpinner.Create(stDot);
  FState := 'ready';
  FErrorMessage := '';
  FSuccessMessage := '';
  FIsGenerating := False;
  FProviderOverride := ProviderOverride;
  FFromTag := FromTag;
  FToTag := ToTag;
  // Initialize async fields
  FAsyncState := osIdle;
  FCurrentOperation := nil;
  FOperationId := '';
  FTerminalWidth := 0;
  FTerminalHeight := 0;
end;

destructor TChangelogModel.Destroy;
begin
  try
    // Cancel any running async operation
    if Assigned(FCurrentOperation) then
    begin
      FCurrentOperation.Cancel;
      FCurrentOperation.Free;
      FCurrentOperation := nil;
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

function TChangelogModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  AsyncChangelogMsg: TAsyncChangelogCompletedMsg;
  NewSpinner: TSpinner;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  // Handle async changelog completion messages
  if Msg is TAsyncChangelogCompletedMsg then
  begin
    AsyncChangelogMsg := TAsyncChangelogCompletedMsg(Msg);
    if AsyncChangelogMsg.OperationId = FOperationId then
    begin
      // This is our operation result
      CancelCurrentOperation; // Clean up
      
      if AsyncChangelogMsg.Success then
      begin
        // Success - display result and quit automatically
        if Pos('SUCCESS:', AsyncChangelogMsg.Result) = 1 then
        begin
          SetSuccess('Changelog generated successfully');
          Result.Cmd := bobaui.QuitCmd;
          Exit;
        end
        else
        begin
          SetError(AsyncChangelogMsg.Result);
          Result.Cmd := bobaui.QuitCmd;
          Exit;
        end;
      end
      else
      begin
        // Error - show error message and quit automatically
        SetError(AsyncChangelogMsg.ErrorMessage);
        Result.Cmd := bobaui.QuitCmd;
        Exit;
      end;
    end;
    Exit; // Don't process other messages this cycle
  end;

  // Handle Ctrl+C interrupt
  if Msg is bobaui.TInterruptMsg then
  begin
    CancelCurrentOperation; // Cancel any running operation
    SetError('Operation cancelled by user');
    Result.Cmd := bobaui.InterruptCmd; // Exit with interrupt signal
    Exit;
  end;

  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Handle 'q' to quit manually
    if (KeyMsg.Key = 'q') or (KeyMsg.Key = 'Q') then
    begin
      CancelCurrentOperation; // Cancel any running operation
      Result.Cmd := bobaui.QuitCmd;
    end;
  end
  else if Msg is bobaui.TComponentTickMsg then
  begin
    if FIsGenerating then  // Show spinner while generating
    begin
      // Update spinner in place during async operation
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
    // Update dimensions in place without recreating model
    FTerminalWidth := WindowMsg.Width;
    FTerminalHeight := WindowMsg.Height;
    
    // If we're waiting to generate and just got window size, start the generation
    if (FAsyncState = osIdle) and (FIsGenerating) and (not Assigned(FCurrentOperation)) then
    begin
      // Start spinner animation and async changelog generation
      StartChangelogGeneration;
      // Execute multiple commands: hide cursor and start ticker
      Result.Cmd := bobaui.BatchCmd([bobaui.HideCursorCmd, FSpinner.Tick]);
    end;
  end;
end;

function TChangelogModel.View: string;
begin
  if FIsGenerating then
    Result := FSpinner.View + ' Generating changelog...'
  else if FErrorMessage <> '' then
    Result := bobastyle.ColorText(#$E2#$9C#$97 + ' ' + FErrorMessage, cRed)  // UTF-8 bytes for ✗ (U+2717)
  else if FSuccessMessage <> '' then
    Result := bobastyle.ColorText(#$E2#$9C#$93 + ' ' + FSuccessMessage, cGreen)  // UTF-8 bytes for ✓ (U+2713)
  else
    Result := '';
end;

function TChangelogModel.Init: bobaui.TCmd;
begin
  Result := FSpinner.Tick;
end;

procedure TChangelogModel.StartGeneration;
begin
  FIsGenerating := True;
  FState := 'generating';
  FErrorMessage := '';
  FSuccessMessage := '';
end;

procedure TChangelogModel.SetSuccess(const Message: string);
begin
  FIsGenerating := False;
  FState := 'success';
  FSuccessMessage := Message;
end;

procedure TChangelogModel.SetError(const Message: string);
begin
  FIsGenerating := False;
  FState := 'error';
  FErrorMessage := Message;
end;

// TChangelogModel helper methods
function TChangelogModel.GenerateOperationId: string;
begin
  Result := 'changelog_op_' + IntToStr(GetTickCount64);
end;

procedure TChangelogModel.StartChangelogGeneration;
begin
  if FAsyncState = osIdle then
  begin
    FOperationId := GenerateOperationId;
    FAsyncState := osGenerating;
    
    // Create and start the async operation
    FCurrentOperation := TAsyncChangelogGeneration.Create(FOperationId, '.', FProviderOverride, FFromTag, FToTag);
    FCurrentOperation.Start;
  end;
end;

procedure TChangelogModel.CancelCurrentOperation;
begin
  if Assigned(FCurrentOperation) then
  begin
    FCurrentOperation.Cancel;
    FCurrentOperation.Free;
    FCurrentOperation := nil;
  end;
  FAsyncState := osIdle;
end;


constructor TGitPalToolContext.Create(const ARepoPath: string; const AFromTag: string; const AToTag: string);
begin
  inherited Create;
  FRepoPath := ExpandFileName(ARepoPath);
  FFromTag := AFromTag;
  FToTag := AToTag;
  
  // Validate repository using git.pas
  if not git.TGitRepository.IsRepository(FRepoPath) then
    raise Exception.Create('Not a git repository: ' + FRepoPath);
end;

destructor TGitPalToolContext.Destroy;
begin
  // Clean up any resources if needed
  // Currently we only have FRepoPath which is a string that cleans itself up
  inherited Destroy;
end;



function TGitPalToolContext.ValidateFilePath(const Path: string): Boolean;
var
  NormalizedPath: string;
begin
  Result := False;
  if Length(Path) = 0 then Exit;
  
  NormalizedPath := ExpandFileName(Path);
  
  // Ensure the path is within our repository
  Result := (Pos(FRepoPath, NormalizedPath) = 1) and
            (Pos('..', Path) = 0) and
            (Length(Path) <= 500);
end;



function TGitPalToolContext.GetGitTags: string;
var
  GitResult: git.TGitResult;
  Tags: TStringList;
  FilteredTags: TStringList;
  i: Integer;
  InRange: Boolean;
begin
  GitResult := git.TGitRepository.GetTags('--sort=-version:refname');
  if not GitResult.Success then
  begin
    Result := 'Error getting git tags: ' + GitResult.ErrorMessage;
    Exit;
  end;
  
  Tags := TStringList.Create;
  FilteredTags := TStringList.Create;
  try
    Tags.Text := GitResult.Output;
    
    // If both from and to are specified, filter to that range
    if (FFromTag <> '') and (FToTag <> '') then
    begin
      InRange := False;
      for i := 0 to Tags.Count - 1 do
      begin
        if Tags[i] = FToTag then
          InRange := True;
        
        if InRange then
          FilteredTags.Add(Tags[i]);
          
        if Tags[i] = FFromTag then
          Break;
      end;
    end
    // If only from is specified, take everything from that tag onwards
    else if FFromTag <> '' then
    begin
      for i := 0 to Tags.Count - 1 do
      begin
        FilteredTags.Add(Tags[i]);
        if Tags[i] = FFromTag then
          Break;
      end;
    end
    // If only to is specified, take everything up to that tag
    else if FToTag <> '' then
    begin
      for i := 0 to Tags.Count - 1 do
      begin
        if Tags[i] = FToTag then
        begin
          FilteredTags.Add(Tags[i]);
          Break;
        end;
        FilteredTags.Add(Tags[i]);
      end;
    end
    // If no range specified, default to 5 most recent tags
    else
    begin
      for i := 0 to Tags.Count - 1 do
      begin
        if i < 5 then
          FilteredTags.Add(Tags[i])
        else
          Break;
      end;
    end;
    
    Result := FilteredTags.Text;
  finally
    Tags.Free;
    FilteredTags.Free;
  end;
end;

function TGitPalToolContext.GetCommitsBetween(const FromRef, ToRef: string): string;
var
  GitResult: git.TGitResult;
begin
  GitResult := git.TGitRepository.GetCommitRange(FromRef, ToRef);
  if GitResult.Success then
    Result := GitResult.Output
  else
    Result := 'Error getting commits: ' + GitResult.ErrorMessage;
end;

function TGitPalToolContext.ReadChangelogFile(const FilePath: string): string;
var
  FileContent: TStringList;
  SafePath: string;
begin
  SafePath := FilePath;
  if SafePath = '' then
    SafePath := 'CHANGELOG.md';
  
  SafePath := FRepoPath + PathDelim + SafePath;
  
  if not ValidateFilePath(SafePath) then
    Exit('Error: Invalid file path');
  
  if not FileExists(SafePath) then
    Exit('File does not exist: ' + ExtractFileName(SafePath));
  
  FileContent := TStringList.Create;
  try
    try
      FileContent.LoadFromFile(SafePath);
      Result := FileContent.Text;
    except
      on E: Exception do
        Result := 'Error reading file: ' + E.Message;
    end;
  finally
    FileContent.Free;
  end;
end;

function TGitPalToolContext.WriteChangelogFile(const Content, FilePath: string): Boolean;
var
  FileContent: TStringList;
  SafePath: string;
begin
  Result := False;
  SafePath := FilePath;
  if SafePath = '' then
    SafePath := 'CHANGELOG.md';
  
  SafePath := FRepoPath + PathDelim + SafePath;
  
  if not ValidateFilePath(SafePath) then
    Exit;
  
  FileContent := TStringList.Create;
  try
    try
      FileContent.Text := Content;
      FileContent.SaveToFile(SafePath);
      Result := True;
    except
      on E: Exception do
      begin
        // Log error but don't expose full path in result
        Result := False;
      end;
    end;
  finally
    FileContent.Free;
  end;
end;

function TGitPalToolContext.ExecuteTool(const ToolCall: TToolCall): TToolResult;
var
  JsonObj: TJSONObject;
  FilePath: string;
  Content: string;
  FromRef, ToRef: string;
begin
  if ToolCall.FunctionName = 'get_git_tags' then
  begin
    Result := CreateToolResult(ToolCall.Id, GetGitTags, False);
  end
  else if ToolCall.FunctionName = 'read_changelog' then
  begin
    FilePath := 'CHANGELOG.md';
    if SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
    begin
      if JsonObj.Find('file_path') <> nil then
        FilePath := JsonObj.Get('file_path', FilePath);
    end;
    Result := CreateToolResult(ToolCall.Id, ReadChangelogFile(FilePath), False);
  end
  else if ToolCall.FunctionName = 'write_changelog' then
  begin
    Content := '';
    FilePath := 'CHANGELOG.md';
    if SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
    begin
      Content := JsonObj.Get('content', '');
      if JsonObj.Find('file_path') <> nil then
        FilePath := JsonObj.Get('file_path', FilePath);
    end;
    
    if WriteChangelogFile(Content, FilePath) then
      Result := CreateToolResult(ToolCall.Id, 'Successfully wrote changelog to ' + ExtractFileName(FilePath), False)
    else
      Result := CreateToolResult(ToolCall.Id, 'Failed to write changelog file', True);
  end
  else if ToolCall.FunctionName = 'get_commits_between' then
  begin
    FromRef := '';
    ToRef := '';
    if SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
    begin
      FromRef := JsonObj.Get('from_tag', '');
      ToRef := JsonObj.Get('to_tag', '');
    end;
    
    if (FromRef = '') or (ToRef = '') then
      Result := CreateToolResult(ToolCall.Id, 'Error: from_tag and to_tag parameters are required', True)
    else
      Result := CreateToolResult(ToolCall.Id, GetCommitsBetween(FromRef, ToRef), False);
  end
  else
  begin
    Result := CreateToolResult(ToolCall.Id, 'Unknown tool: ' + ToolCall.FunctionName, True);
  end;
  
  // Always set the function name for tracking
  Result.FunctionName := ToolCall.FunctionName;
end;

function TGitPalToolContext.GetAvailableTools: TToolFunctionArray;
begin
  Result := nil;
  SetLength(Result, 4);
  
  // Tool 1: Get git tags
  Result[0].Name := 'get_git_tags';
  Result[0].Description := 'Get list of git tags sorted by version (newest first)';
  SetLength(Result[0].Parameters, 0);
  
  // Tool 2: Get commits between tags
  Result[1].Name := 'get_commits_between';
  Result[1].Description := 'Get the list of commits between two git references/tags. Use this to see what changes were made in a version.';
  SetLength(Result[1].Parameters, 2);
  Result[1].Parameters[0].Name := 'from_tag';
  Result[1].Parameters[0].ParamType := tptString;
  Result[1].Parameters[0].Description := 'Starting git reference/tag (older version)';
  Result[1].Parameters[0].Required := True;
  Result[1].Parameters[1].Name := 'to_tag';
  Result[1].Parameters[1].ParamType := tptString;
  Result[1].Parameters[1].Description := 'Ending git reference/tag (newer version)';
  Result[1].Parameters[1].Required := True;
  
  // Tool 3: Read changelog file
  Result[2].Name := 'read_changelog';
  Result[2].Description := 'Read the current changelog file';
  SetLength(Result[2].Parameters, 1);
  Result[2].Parameters[0].Name := 'file_path';
  Result[2].Parameters[0].ParamType := tptString;
  Result[2].Parameters[0].Description := 'Path to changelog file (default: CHANGELOG.md)';
  Result[2].Parameters[0].Required := False;
  
  // Tool 4: Write changelog file
  Result[3].Name := 'write_changelog';
  Result[3].Description := 'Write content to the changelog file';
  SetLength(Result[3].Parameters, 2);
  Result[3].Parameters[0].Name := 'content';
  Result[3].Parameters[0].ParamType := tptString;
  Result[3].Parameters[0].Description := 'Complete changelog content to write';
  Result[3].Parameters[0].Required := True;
  Result[3].Parameters[1].Name := 'file_path';
  Result[3].Parameters[1].ParamType := tptString;
  Result[3].Parameters[1].Description := 'Path to changelog file (default: CHANGELOG.md)';
  Result[3].Parameters[1].Required := False;
end;

// Thread-safe procedure for changelog generation
function GenerateChangelogAsync(const RepoPath: string; const ProviderOverride: string = ''; const FromTag: string = ''; const ToTag: string = ''): string;
var
  Config: TGitPalConfig;
  Registry: TProviderRegistry;
  ProviderName: AnsiString;
  Provider: ILLMProvider;
  ToolContext: IToolContext;
  Messages: array of TLLMMessage;
  Tools: TToolFunctionArray;
  Response: TLLMChatCompletionResponse;
begin
  Result := '';
  
  try
    // Load configuration
    Config := TGitPalConfig.Create;
    Registry := TProviderRegistry.Create;
    try
      if not Config.LoadConfig then
      begin
        Result := 'Error: No configuration found. Please run "gitpal setup" first.';
        Exit;
      end;
      
      // Determine which provider to use
      if ProviderOverride <> '' then
        ProviderName := AnsiString(ProviderOverride)
      else
        ProviderName := Config.DefaultProvider;
      
      if ProviderName = '' then
      begin
        Result := 'Error: No provider specified and no default provider configured.';
        Exit;
      end;
      
      // Create provider from config
      try
        Provider := Registry.CreateProviderFromConfig(Config, ProviderName);
      except
        on E: Exception do
        begin
          Result := 'Error: ' + E.Message;
          Exit;
        end;
      end;
      
      // Check if provider supports tool calling
      if not Provider.SupportsToolCalling then
      begin
        Result := 'Error: Provider "' + string(ProviderName) + '" does not support tool calling.';
        Exit;
      end;
      
      // Create tool context (validates git repository)
      try
        ToolContext := TGitPalToolContext.Create(RepoPath, FromTag, ToTag);
      except
        on E: Exception do
        begin
          Result := 'Error: ' + E.Message + '. Please run this command from within a git repository.';
          Exit;
        end;
      end;
      
      // Setup conversation - combine system and user into single user message for Gemini
      SetLength(Messages, 1);
      
      Messages[0].Role := lmrUser;
      Messages[0].Content := AnsiString('You are an expert software developer and changelog maintainer. ') +
                             AnsiString('Your task is to analyze a git repository and update its CHANGELOG.md file. ') +
                             AnsiString('Follow these guidelines:') + #10 +
                             AnsiString('- Use Keep-a-Changelog format (https://keepachangelog.com/)') + #10 +
                             AnsiString('- Group changes by type: Added, Changed, Deprecated, Removed, Fixed, Security') + #10 +
                             AnsiString('- Write clear, user-focused descriptions') + #10 +
                             AnsiString('- Include version numbers and dates') + #10 +
                             AnsiString('- Only add entries for versions that are missing from the changelog') + #10 +
                             AnsiString('- Preserve existing changelog content and format') + #10 +
                             AnsiString('- If no CHANGELOG.md exists, create one with proper header and format') + #10 + #10 +
                             AnsiString('Please update my CHANGELOG.md file by analyzing my git tags and adding entries for any missing versions. ') +
                             AnsiString('You have these tools available:') + #10 +
                             AnsiString('- get_git_tags: Get all git tags') + #10 +
                             AnsiString('- get_commits_between: Get commits between two tags/refs') + #10 +
                             AnsiString('- read_changelog: Read existing CHANGELOG.md') + #10 +
                             AnsiString('- write_changelog: Write updated CHANGELOG.md') + #10 + #10 +
                             AnsiString('Please start by calling the get_git_tags function to see what versions exist. ') +
                             AnsiString('Note: The tags returned may be limited to a specific range or the most recent versions. ') +
                             AnsiString('After that, call read_changelog to see the current content. ') + 
                             AnsiString('Then analyze what''s needed and use the other tools as necessary. ') + #10 + #10 +
                             AnsiString('IMPORTANT: Please call only ONE function at a time and wait for the result before calling the next function. ') +
                             AnsiString('Focus on creating meaningful, user-readable changelog entries rather than just listing commit messages.');
      
      // Get available tools and execute conversation
      Tools := ToolContext.GetAvailableTools;
      
      // Execute tool-calling conversation using configured model
      Response := Provider.ChatCompletionWithTools(
        Provider.GetDefaultModel,
        Messages,
        Tools,
        ToolContext,
        0.3,    // Low temperature for consistent formatting
        3000    // Higher token limit for detailed changelogs
      );
      
      // Process result
      if Length(Response.Choices) > 0 then
      begin
        if Length(Response.Choices[0].Message.Content) > 0 then
        begin
          Result := 'SUCCESS:' + Response.Choices[0].Message.Content;
        end
        else
        begin
          Result := 'Error: No response content received from AI';
        end;
      end
      else
      begin
        Result := 'Error: No response received from AI';
      end;
      
    finally
      Config.Free;
      Registry.Free;
    end;
    
  except
    on E: Exception do
    begin
      Result := 'Error generating changelog: ' + E.Message;
    end;
  end;
end;

procedure RunChangelogCommand(const ProviderOverride: string = ''; const FromTag: string = ''; const ToTag: string = '');
var
  Prog: TBobaUIProgram;
  Model: TChangelogModel;
begin
  // Create model that will show spinner and generate changelog async
  Model := TChangelogModel.Create(ProviderOverride, FromTag, ToTag);
  Model.StartGeneration;
  
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set the global program reference for async operations
  GProgram := Prog;
  
  try
    Prog.Run;
  except
    on E: bobaui.EBobaUIInterrupted do
    begin
      writeln('');
      writeln('Operation cancelled by user.');
      Halt(130); // Standard exit code for SIGINT
    end;
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