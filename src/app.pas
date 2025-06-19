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
end;

constructor TSelectModel.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  FTerminalWidth := AWidth;
  FTerminalHeight := AHeight;
end;

destructor TSelectModel.Destroy;
begin
  inherited Destroy;
end;

function TSelectModel.View: string;
begin
  Result := 'Hello world';
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
    end;
  end
  else if Msg is bobaui.TWindowSizeMsg then
  begin
    WindowMsg := bobaui.TWindowSizeMsg(Msg);
    if (WindowMsg.Width <> FTerminalWidth) or (WindowMsg.Height <> FTerminalHeight) then
    begin
      NewModel := TSelectModel.Create(WindowMsg.Width, WindowMsg.Height);
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
  Prog := TBobaUIProgram.Create(Model, bobaui.dmInline);
  
  // Set global program reference
  GlobalProgram := Pointer(Prog);
  
  Prog.Run;
  
  // Cleanup
  GlobalProgram := nil;
  Prog.Free;
end.