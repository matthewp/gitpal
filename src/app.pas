program app;

{$mode objfpc}
{$codepage UTF8}
{$H+}

uses
  bobaui,
  bobastyle,
  bobacomponents,
  SysUtils;

type
  TCommitModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FList: bobacomponents.TList;
    procedure OnItemSelect(Index: integer; const Item: string);
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: integer); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

constructor TCommitModel.Create;
begin
  inherited Create;
  FTerminalWidth := 80;
  FTerminalHeight := 24;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.OnSelect := @OnItemSelect;
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

constructor TCommitModel.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  FTerminalWidth := AWidth;
  FTerminalHeight := AHeight;
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.OnSelect := @OnItemSelect;
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

destructor TCommitModel.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCommitModel.OnItemSelect(Index: integer; const Item: string);
begin
  if Index = 0 then
    writeln('Accept selected')
  else
    writeln('Decline selected');
end;

function TCommitModel.View: string;
begin
  Result := AnsiString('Commit changes?') + #10 + #10 + FList.View;
end;

function TCommitModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
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
    // Handle Enter to select and quit
    else if KeyMsg.Key = #13 then
    begin
      OnItemSelect(FList.SelectedIndex, string(FList.Items[FList.SelectedIndex]));
      Result.Cmd := bobaui.QuitCmd;
    end
    else
    begin
      // Delegate other keys to the list component
      NewList := FList.Update(Msg);
      if NewList <> FList then
      begin
        NewModel := TCommitModel.Create(FTerminalWidth, FTerminalHeight);
        NewModel.FList.Free;
        NewModel.FList := NewList;
        Result.Model := NewModel;
      end;
    end;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    if (WindowMsg.Width <> FTerminalWidth) or (WindowMsg.Height <> FTerminalHeight) then
    begin
      NewModel := TCommitModel.Create(WindowMsg.Width, WindowMsg.Height);
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
  writeln('  --help, -h   Show this help message');
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
  writeln('  --help, -h   Show this help message');
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

procedure RunCommitCommand;
var
  Prog: TBobaUIProgram;
  Model: TCommitModel;
begin
  Model := TCommitModel.Create(80, 24);
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
  i: integer;
  ShowMainHelp: boolean;
begin
  ShowMainHelp := false;
  Command := AnsiString('');
  
  // Parse command line arguments
  if ParamCount = 0 then
    ShowMainHelp := true
  else
  begin
    for i := 1 to ParamCount do
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
      else if (Command = AnsiString('')) and (ParamStr(i)[1] <> '-') then
        Command := AnsiString(ParamStr(i));
    end;
  end;
  
  // Show help or execute command
  if ShowMainHelp then
  begin
    ShowHelp;
    Exit;
  end;
  
  if Command = AnsiString('commit') then
    RunCommitCommand
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