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
  TSelectModel = class(bobaui.TModel)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FSelectedIndex: integer;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: integer); overload;
    destructor Destroy; override;
    function View: string; override;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
  end;

constructor TSelectModel.Create;
begin
  inherited Create;
  FTerminalWidth := 80;
  FTerminalHeight := 24;
  FSelectedIndex := 0;
end;

constructor TSelectModel.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  FTerminalWidth := AWidth;
  FTerminalHeight := AHeight;
  FSelectedIndex := 0;
end;

destructor TSelectModel.Destroy;
begin
  inherited Destroy;
end;

function TSelectModel.View: string;
var
  Options: array[0..1] of AnsiString;
  I: integer;
begin
  Options[0] := AnsiString('Accept');
  Options[1] := AnsiString('Decline');
  
  Result := AnsiString('Commit changes?') + #10 + #10;
  
  for I := 0 to 1 do
  begin
    if I = FSelectedIndex then
      Result := Result + AnsiString('> ')
    else
      Result := Result + AnsiString('  ');
    Result := Result + Options[I] + #10;
  end;
end;

function TSelectModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  NewModel: TSelectModel;
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
    // Handle up arrow or 'k' to move up
    else if KeyMsg.IsUpArrow or (KeyMsg.Key = 'k') or (KeyMsg.Key = 'K') then
    begin
      if FSelectedIndex > 0 then
        FSelectedIndex := FSelectedIndex - 1;
    end
    // Handle down arrow or 'j' to move down
    else if KeyMsg.IsDownArrow or (KeyMsg.Key = 'j') or (KeyMsg.Key = 'J') then
    begin
      if FSelectedIndex < 1 then
        FSelectedIndex := FSelectedIndex + 1;
    end
    // Handle Enter to select option
    else if KeyMsg.Key = #13 then
    begin
      if FSelectedIndex = 0 then
        writeln('Accept selected')
      else
        writeln('Decline selected');
      Result.Cmd := bobaui.QuitCmd;
    end;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    if (WindowMsg.Width <> FTerminalWidth) or (WindowMsg.Height <> FTerminalHeight) then
    begin
      NewModel := TSelectModel.Create(WindowMsg.Width, WindowMsg.Height);
      NewModel.FSelectedIndex := FSelectedIndex;
      Result.Model := NewModel;
    end;
  end;
end;

var
  Prog: TBobaUIProgram;
  Model: TSelectModel;
  Display: bobaui.TDisplay;
begin
  Model := TSelectModel.Create(80, 24);
  Prog := TBobaUIProgram.Create(Model, bobaui.dmFullscreen);
  
  // Set global program reference
  GlobalProgram := Pointer(Prog);
  
  Prog.Run;
  
  // Cleanup
  GlobalProgram := nil;
  Prog.Free;
end.