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
  Process;

{$I gitpal-version.inc}

type
  TCommitModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FList: bobacomponents.TList;
    FCommitMessage: string;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: integer); overload;
    constructor Create(AWidth, AHeight: integer; const ACommitMessage: string); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

constructor TCommitModel.Create;
begin
  inherited Create;
  FTerminalWidth := 80;
  FTerminalHeight := 24;
  FCommitMessage := AnsiString('');
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

constructor TCommitModel.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  FTerminalWidth := AWidth;
  FTerminalHeight := AHeight;
  FCommitMessage := AnsiString('');
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

constructor TCommitModel.Create(AWidth, AHeight: integer; const ACommitMessage: string);
begin
  inherited Create;
  FTerminalWidth := AWidth;
  FTerminalHeight := AHeight;
  FCommitMessage := ACommitMessage;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

destructor TCommitModel.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;


function TCommitModel.View: string;
var
  Style: bobastyle.TStyle;
  BorderedMessage: string;
  PaddedContent: string;
  Sections: array of string;
begin
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
  
  // Use JoinVertical to properly space sections
  SetLength(Sections, 5);
  Sections[0] := AnsiString('Generated commit message:');
  Sections[1] := AnsiString(''); // Empty line above box
  Sections[2] := BorderedMessage;
  Sections[3] := AnsiString(''); // Empty line below box
  Sections[4] := AnsiString('Accept this commit message?') + #10 + #10 + FList.View;
  
  Result := bobastyle.JoinVertical(Sections);
end;

function TCommitModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  ListSelectionMsg: bobacomponents.TListSelectionMsg;
  NewModel: TCommitModel;
  NewList: bobacomponents.TList;
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
    end
    else
    begin
      // Delegate keys to the list component
      NewList := FList.Update(Msg);
      if NewList <> FList then
      begin
        NewModel := TCommitModel.Create(FTerminalWidth, FTerminalHeight, FCommitMessage);
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
    
    // Handle selection and quit
    if ListSelectionMsg.SelectedIndex = 0 then
      writeln('Accept selected')
    else
      writeln('Decline selected');
      
    Result.Cmd := bobaui.QuitCmd;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    if (WindowMsg.Width <> FTerminalWidth) or (WindowMsg.Height <> FTerminalHeight) then
    begin
      NewModel := TCommitModel.Create(WindowMsg.Width, WindowMsg.Height, FCommitMessage);
      NewModel.FList.SelectedIndex := FList.SelectedIndex;
      Result.Model := NewModel;
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
    
    // Call ChatCompletion
    Response := GeminiProvider.ChatCompletion(
      GeminiProvider.GetDefaultModel,
      Messages,
      0.3,    // Lower temperature for more consistent results
      400     // Max tokens for detailed commit message with description
    );
    
    // Extract the response
    if Length(Response.Choices) > 0 then
      Result := Response.Choices[0].Message.Content
    else
      Result := AnsiString('Error: No response from LLM');
      
  finally
    GeminiProvider.Free;
  end;
end;

procedure RunCommitCommand(const CustomPrompt: string = '');
var
  Prog: TBobaUIProgram;
  Model: TCommitModel;
  DiffContent: string;
  CommitMessage: string;
begin
  // Check if there are staged changes
  DiffContent := GetGitDiff;
  if DiffContent = '' then
  begin
    writeln('No staged changes found. Use "git add" to stage changes before generating a commit message.');
    Exit;
  end;
  
  // Generate commit message using LLM
  writeln('Analyzing staged changes and generating commit message...');
  CommitMessage := GenerateCommitMessage(DiffContent, CustomPrompt);
  
  if Pos('Error:', CommitMessage) = 1 then
  begin
    writeln(CommitMessage);
    Exit;
  end;
  
  // Create model with the generated commit message
  Model := TCommitModel.Create(80, 24, CommitMessage);
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set global program reference
  GlobalProgram := Pointer(Prog);
  
  Prog.Run;
  
  // Cleanup
  GlobalProgram := nil;
  Prog.Free;
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