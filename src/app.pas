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
    FList: bobacomponents.TList;
    procedure OnItemSelect(Index: integer; const Item: string);
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
  
  FList := bobacomponents.TList.Create;
  FList.AddItem(AnsiString('Accept'));
  FList.AddItem(AnsiString('Decline'));
  FList.OnSelect := @OnItemSelect;
  FList.Width := 30;
  FList.Height := 5;
  FList.ShowBorder := false;
end;

constructor TSelectModel.Create(AWidth, AHeight: integer);
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

destructor TSelectModel.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TSelectModel.OnItemSelect(Index: integer; const Item: string);
begin
  if Index = 0 then
    writeln('Accept selected')
  else
    writeln('Decline selected');
end;

function TSelectModel.View: string;
begin
  Result := AnsiString('Commit changes?') + #10 + #10 + FList.View;
end;

function TSelectModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  KeyMsg: bobaui.TKeyMsg;
  WindowMsg: bobaui.TWindowSizeMsg;
  NewModel: TSelectModel;
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
        NewModel := TSelectModel.Create(FTerminalWidth, FTerminalHeight);
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
      NewModel := TSelectModel.Create(WindowMsg.Width, WindowMsg.Height);
      NewModel.FList.SelectedIndex := FList.SelectedIndex;
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