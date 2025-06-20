{$codepage UTF8}
unit bobacomponents;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils,
  DateUtils,
  bobaui,
  bobastyle;

type
  // List selection message for component communication
  TListSelectionMsg = class(bobaui.TMsg)
  private
    FSelectedIndex: Integer;
    FSelectedItem: string;
    FListId: string;
  public
    constructor Create(AIndex: Integer; const AItem: string; const AListId: string = '');
    property SelectedIndex: Integer read FSelectedIndex;
    property SelectedItem: string read FSelectedItem;
    property ListId: string read FListId;
  end;

  TSpinnerType = (
    stLine,
    stDot,
    stMiniDot,
    stJump,
    stPulse,
    stPoints,
    stGlobe,
    stMoon,
    stMonkey,
    stMeter,
    stHamburger
  );

  // Menu command for individual selectable items within dropdowns
  TDOSMenuCommand = class
  private
    FText: string;
    FHotKey: char;
    FEnabled: Boolean;
    FIsSeparator: Boolean;
    FAction: string; // Optional action identifier
  public
    constructor Create(const AText: string; AHotKey: char = #0; AEnabled: Boolean = True);
    constructor CreateSeparator;
    
    property Text: string read FText write FText;
    property HotKey: char read FHotKey write FHotKey;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IsSeparator: Boolean read FIsSeparator;
    property Action: string read FAction write FAction;
    
    function GetDisplayText: string;
    function MatchesKey(Key: char): Boolean;
  end;

  TDOSMenuCommandArray = array of TDOSMenuCommand;

  // Dropdown menu that contains multiple menu commands
  TDOSDropdown = class
  private
    FCommands: TDOSMenuCommandArray;
    FSelectedIndex: Integer;
    FTitle: string;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(const ATitle: string = '');
    destructor Destroy; override;
    
    procedure AddCommand(const AText: string; AHotKey: char = #0; AEnabled: Boolean = True; const AAction: string = '');
    procedure AddSeparator;
    procedure ClearCommands;
    function GetCommandCount: Integer;
    function GetSelectedCommand: TDOSMenuCommand;
    
    property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    
    function View: string;
    function Update(const Msg: TMsg): TDOSDropdown;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectFirst;
    procedure SelectLast;
    function FindCommandByKey(Key: char): Integer;
  end;

  // Spinner component for loading animations
  TSpinner = class
  private
    FSpinnerType: TSpinnerType;
    FFrames: array of string;
    FFrame: integer;
    FId: integer;
    FTag: integer;
    FStyle: TStyle;
    procedure SetFramesForType(SpinnerType: TSpinnerType);
  public
    constructor Create;
    constructor Create(SpinnerType: TSpinnerType);
    destructor Destroy; override;
    function Update(const Msg: TMsg): TSpinner;
    function View: string;
    function Tick: TCmd;
    function WithStyle(AStyle: TStyle): TSpinner;
    property SpinnerType: TSpinnerType read FSpinnerType write FSpinnerType;
    property Style: TStyle read FStyle write FStyle;
  end;

  // Enhanced menu item for DOS-style menus that can have dropdowns
  TDOSMenuItem = class
  private
    FText: string;
    FHotKey: char;
    FEnabled: Boolean;
    FDropdown: TDOSDropdown;
    FHasDropdown: Boolean;
  public
    constructor Create(const AText: string; AHotKey: char; AEnabled: Boolean = True);
    destructor Destroy; override;
    
    property Text: string read FText write FText;
    property HotKey: char read FHotKey write FHotKey;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Dropdown: TDOSDropdown read FDropdown;
    property HasDropdown: Boolean read FHasDropdown;
    
    procedure SetDropdown(ADropdown: TDOSDropdown);
    function MatchesKey(Key: char): Boolean;
  end;

  TDOSMenuItemArray = array of TDOSMenuItem;

  // Enhanced DOS-style top menu component with dropdown support
  TDOSMenu = class
  private
    FItems: TDOSMenuItemArray;
    FSelectedIndex: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FActiveDropdown: TDOSDropdown;
    FDropdownVisible: Boolean;
    FDropdownPosition: TRect;
    FPendingAction: string;
    FHasPendingAction: Boolean;
    function GetSelectedItem: TDOSMenuItem;
    function CopyDropdown(Source: TDOSDropdown): TDOSDropdown;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Menu management
    procedure AddItem(const Text: string; Key: char; Enabled: Boolean = True);
    procedure AddItemWithDropdown(const Text: string; Key: char; ADropdown: TDOSDropdown; Enabled: Boolean = True);
    procedure ClearItems;
    function GetItemCount: Integer;
    
    // Dropdown management
    procedure ShowDropdown;
    procedure HideDropdown;
    function HasActiveDropdown: Boolean;
    
    // Properties
    property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
    property SelectedItem: TDOSMenuItem read GetSelectedItem;
    property ActiveDropdown: TDOSDropdown read FActiveDropdown;
    property DropdownVisible: Boolean read FDropdownVisible;
    property DropdownPosition: TRect read FDropdownPosition;
    
    // Methods
    function View: string;
    function Update(const Msg: TMsg): TDOSMenu;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectFirst;
    procedure SelectLast;
    function GetSelectedItemPosition: Integer;
    function GetDropdownPosition: TRect;
    function FindItemByKey(Key: char): Integer;
    
    // Query methods for dropdown state (safe for use in Update)
    function ShouldShowDropdown: Boolean;
    function GetDropdownContent: string;
    function GetSelectedAction: string;
    function HasPendingAction: Boolean;
    procedure ClearPendingAction;
  end;

  // Input field component for text entry
  TInputField = class
  private
    FText: string;
    FPlaceholder: string;
    FWidth: integer;
    FFocused: boolean;
    FCursorPos: integer;
    FMaxLength: integer;
    
    function GetDisplayText: string;
  public
    constructor Create;
    
    // Properties
    property Text: string read FText write FText;
    property Placeholder: string read FPlaceholder write FPlaceholder;
    property Width: integer read FWidth write FWidth;
    property Focused: boolean read FFocused write FFocused;
    property CursorPos: integer read FCursorPos write FCursorPos;
    property MaxLength: integer read FMaxLength write FMaxLength;
    
    // Methods
    function View: string;
    function Update(const Msg: TMsg): TInputField;
    procedure Clear;
    procedure MoveCursorLeft;
    procedure MoveCursorRight;
    procedure MoveCursorHome;
    procedure MoveCursorEnd;
    procedure DeleteChar;
    procedure Backspace;
    procedure InsertChar(Ch: char);
  end;

  // List component for selecting items
  TList = class
  private
    FItems: TStringArray;
    FSelectedIndex: integer;
    FFocused: boolean;
    FWidth: integer;
    FHeight: integer;
    FTitle: string;
    FSelectionIndicator: string;
    FListId: string;
    FHasPendingSelection: Boolean;
    FPendingSelectionIndex: Integer;
    FPendingSelectionItem: string;
    FShowBorder: boolean;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FSelectedColor: TColor;
    FScrollOffset: integer;
    function GetSelectedItem: string;
    function GetItemCount: integer;
    function GetVisibleItems: integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Properties
    property Items: TStringArray read FItems write FItems;
    property SelectedIndex: integer read FSelectedIndex write FSelectedIndex;
    property Focused: boolean read FFocused write FFocused;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property Title: string read FTitle write FTitle;
    property SelectionIndicator: string read FSelectionIndicator write FSelectionIndicator;
    property ListId: string read FListId write FListId;
    property ShowBorder: boolean read FShowBorder write FShowBorder;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property SelectedItem: string read GetSelectedItem;
    property ItemCount: integer read GetItemCount;
    
    // Methods
    function View: string;
    function Update(const Msg: bobaui.TMsg): TList;
    procedure AddItem(const Item: string);
    procedure ClearItems;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectFirst;
    procedure SelectLast;
    
    // Selection handling for message-based communication
    function HasPendingSelection: Boolean;
    function GetPendingSelection: TListSelectionMsg;
    procedure ClearPendingSelection;
  end;

// Component command functions
function ListSelectionCmd(Index: Integer; const Item: string; const ListId: string = ''): bobaui.TCmd;

var
  NextSpinnerId: integer = 0;

implementation

// TListSelectionMsg implementation

constructor TListSelectionMsg.Create(AIndex: Integer; const AItem: string; const AListId: string = '');
begin
  inherited Create;
  FSelectedIndex := AIndex;
  FSelectedItem := AItem;
  FListId := AListId;
end;

// Component command functions

// Global variables for ListSelectionCmd parameters
var
  PendingListSelectionIndex: Integer = 0;
  PendingListSelectionItem: string = '';
  PendingListSelectionId: string = '';

function DoListSelectionCmd: bobaui.TMsg; cdecl;
begin
  Result := TListSelectionMsg.Create(PendingListSelectionIndex, PendingListSelectionItem, PendingListSelectionId);
end;

function ListSelectionCmd(Index: Integer; const Item: string; const ListId: string = ''): bobaui.TCmd;
begin
  PendingListSelectionIndex := Index;
  PendingListSelectionItem := Item;
  PendingListSelectionId := ListId;
  Result := @DoListSelectionCmd;
end;

// TDOSMenuCommand implementation

constructor TDOSMenuCommand.Create(const AText: string; AHotKey: char; AEnabled: Boolean);
begin
  inherited Create;
  FText := AText;
  FHotKey := AHotKey;
  FEnabled := AEnabled;
  FIsSeparator := False;
  FAction := AnsiString('');
end;

constructor TDOSMenuCommand.CreateSeparator;
begin
  inherited Create;
  FText := AnsiString('');
  FHotKey := #0;
  FEnabled := False;
  FIsSeparator := True;
  FAction := AnsiString('');
end;

function TDOSMenuCommand.GetDisplayText: string;
var
  HotKeyPos: Integer;
begin
  if FIsSeparator then
  begin
    Result := '─────────────────────';
    Exit;
  end;
  
  Result := FText;
  
  // Add hotkey indicator if present
  if (FHotKey <> #0) and (FHotKey <> ' ') then
  begin
    HotKeyPos := Pos(UpCase(FHotKey), UpCase(FText));
    if HotKeyPos > 0 then
    begin
      // Underline the hotkey character
      Result := Copy(FText, 1, HotKeyPos - 1) + 
               #27'[4m' + FText[HotKeyPos] + #27'[24m' + 
               Copy(FText, HotKeyPos + 1, Length(FText));
    end;
  end;
end;

function TDOSMenuCommand.MatchesKey(Key: char): Boolean;
begin
  Result := (FHotKey <> #0) and (UpCase(FHotKey) = UpCase(Key)) and FEnabled and not FIsSeparator;
end;

// TDOSDropdown implementation

constructor TDOSDropdown.Create(const ATitle: string);
begin
  inherited Create;
  FTitle := ATitle;
  FSelectedIndex := 0;
  FWidth := 20;
  FHeight := 10;
  SetLength(FCommands, 0);
end;

destructor TDOSDropdown.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FCommands) - 1 do
    if Assigned(FCommands[I]) then
      FCommands[I].Free;
  SetLength(FCommands, 0);
  inherited Destroy;
end;

procedure TDOSDropdown.AddCommand(const AText: string; AHotKey: char; AEnabled: Boolean; const AAction: string);
var
  NewLength: Integer;
  Command: TDOSMenuCommand;
begin
  Command := TDOSMenuCommand.Create(AText, AHotKey, AEnabled);
  Command.Action := AAction;
  
  NewLength := Length(FCommands);
  SetLength(FCommands, NewLength + 1);
  FCommands[NewLength] := Command;
  
  // Update width based on text length
  if Length(AText) + 4 > FWidth then
    FWidth := Length(AText) + 4;
end;

procedure TDOSDropdown.AddSeparator;
var
  NewLength: Integer;
begin
  NewLength := Length(FCommands);
  SetLength(FCommands, NewLength + 1);
  FCommands[NewLength] := TDOSMenuCommand.CreateSeparator;
end;

procedure TDOSDropdown.ClearCommands;
var
  I: Integer;
begin
  for I := 0 to Length(FCommands) - 1 do
    if Assigned(FCommands[I]) then
      FCommands[I].Free;
  SetLength(FCommands, 0);
  FSelectedIndex := 0;
end;

function TDOSDropdown.GetCommandCount: Integer;
begin
  Result := Length(FCommands);
end;

function TDOSDropdown.GetSelectedCommand: TDOSMenuCommand;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FCommands)) then
    Result := FCommands[FSelectedIndex]
  else
    Result := nil;
end;

function TDOSDropdown.View: string;
var
  Content: string;
  DialogStyle: TStyle;
  CommandList: array of string;
  I: Integer;
  Command: TDOSMenuCommand;
  DisplayText: string;
  DebugFile: TextFile;
begin
  // Debug logging
  AssignFile(DebugFile, 'debug_dropdown.log');
  if FileExists('debug_dropdown.log') then
    Append(DebugFile)
  else
    Rewrite(DebugFile);
  
  WriteLn(DebugFile, '=== TDOSDropdown.View START ===');
  WriteLn(DebugFile, 'Length(FCommands): ', Length(FCommands));
  WriteLn(DebugFile, 'FSelectedIndex: ', FSelectedIndex);
  WriteLn(DebugFile, 'FWidth: ', FWidth);
  WriteLn(DebugFile, 'FHeight: ', FHeight);
  
  SetLength(CommandList, Length(FCommands));
  
  for I := 0 to Length(FCommands) - 1 do
  begin
    Command := FCommands[I];
    DisplayText := Command.GetDisplayText;
    WriteLn(DebugFile, 'Command ', I, ': "', Command.Text, '" -> "', DisplayText, '"');
    WriteLn(DebugFile, 'Command ', I, ' IsSeparator: ', Command.IsSeparator);
    WriteLn(DebugFile, 'Command ', I, ' Enabled: ', Command.Enabled);
    
    if I = FSelectedIndex then
    begin
      // Selected item: white text on blue background (DOS style)
      CommandList[I] := GetRGBColorCode(RGB(255, 255, 255)) + GetRGBColorCode(RGB(0, 0, 170), True) + 
                       ' ' + DisplayText + ' ' + ResetColor;
      WriteLn(DebugFile, 'Command ', I, ' is SELECTED');
    end
    else if Command.Enabled and not Command.IsSeparator then
    begin
      // Enabled item: black text on gray background
      CommandList[I] := ' ' + DisplayText + ' ';
      WriteLn(DebugFile, 'Command ', I, ' is ENABLED');
    end
    else if Command.IsSeparator then
    begin
      // Separator: dark gray line
      CommandList[I] := GetRGBColorCode(RGB(128, 128, 128)) + DisplayText + ResetColor;
      WriteLn(DebugFile, 'Command ', I, ' is SEPARATOR');
    end
    else
    begin
      // Disabled item: dark gray text
      CommandList[I] := GetRGBColorCode(RGB(128, 128, 128)) + ' ' + DisplayText + ' ' + ResetColor;
      WriteLn(DebugFile, 'Command ', I, ' is DISABLED');
    end;
    
    WriteLn(DebugFile, 'CommandList[', I, '] length: ', Length(CommandList[I]));
  end;
  
  Content := JoinVertical(CommandList);
  WriteLn(DebugFile, 'Content length after JoinVertical: ', Length(Content));
  
  // Use TStyle for borders with DOS-style gray background
  DialogStyle := TStyle.Create;
  try
    DialogStyle.BorderStyle := bsSingle;
    DialogStyle.BorderColor := cBlack;
    DialogStyle.BorderBackgroundColorRGB := RGB(192, 192, 192);
    DialogStyle.UseRGBBorderBackground := True;
    DialogStyle.BackgroundColorRGB := RGB(192, 192, 192);
    DialogStyle.UseRGBBackground := True;
    DialogStyle.ContentColor := cBlack;
    DialogStyle.Content := Content;
    DialogStyle.Width := FWidth;
    DialogStyle.Height := Length(FCommands) + 2; // +2 for borders
    
    WriteLn(DebugFile, 'DialogStyle.Width: ', DialogStyle.Width);
    WriteLn(DebugFile, 'DialogStyle.Height: ', DialogStyle.Height);
    WriteLn(DebugFile, 'DialogStyle.Content length: ', Length(DialogStyle.Content));
    
    Result := DialogStyle.Render;
    WriteLn(DebugFile, 'Final Result length: ', Length(Result));
  finally
    DialogStyle.Free;
  end;
  
  WriteLn(DebugFile, '=== TDOSDropdown.View END ===');
  CloseFile(DebugFile);
end;

function TDOSDropdown.Update(const Msg: TMsg): TDOSDropdown;
var
  KeyMsg: TKeyMsg;
  NewDropdown: TDOSDropdown;
  I, FoundIndex: Integer;
begin
  Result := Self; // Default: no change
  
  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    
    // Create new instance for immutable updates
    NewDropdown := TDOSDropdown.Create(FTitle);
    NewDropdown.FWidth := FWidth;
    NewDropdown.FHeight := FHeight;
    NewDropdown.FSelectedIndex := FSelectedIndex;
    
    // Copy commands
    SetLength(NewDropdown.FCommands, Length(FCommands));
    for I := 0 to Length(FCommands) - 1 do
    begin
      if FCommands[I].IsSeparator then
        NewDropdown.FCommands[I] := TDOSMenuCommand.CreateSeparator
      else
      begin
        NewDropdown.FCommands[I] := TDOSMenuCommand.Create(FCommands[I].Text, FCommands[I].HotKey, FCommands[I].Enabled);
        NewDropdown.FCommands[I].Action := FCommands[I].Action;
      end;
    end;
    
    if KeyMsg.IsUpArrow then
    begin
      NewDropdown.SelectPrevious;
      Result := NewDropdown;
    end
    else if KeyMsg.IsDownArrow then
    begin
      NewDropdown.SelectNext;
      Result := NewDropdown;
    end
    else
    begin
      // Check for hotkeys
      FoundIndex := NewDropdown.FindCommandByKey(KeyMsg.Key);
      if FoundIndex >= 0 then
      begin
        NewDropdown.FSelectedIndex := FoundIndex;
        Result := NewDropdown;
      end
      else
      begin
        // If we didn't find a match, free the new dropdown
        NewDropdown.Free;
      end;
    end;
    
    // If we didn't create a new dropdown, free it
    if Result = Self then
      NewDropdown.Free;
  end;
end;

procedure TDOSDropdown.SelectNext;
var
  StartIndex: Integer;
begin
  if Length(FCommands) = 0 then
    Exit;
    
  StartIndex := FSelectedIndex;
  repeat
    FSelectedIndex := (FSelectedIndex + 1) mod Length(FCommands);
  until (FCommands[FSelectedIndex].Enabled and not FCommands[FSelectedIndex].IsSeparator) or (FSelectedIndex = StartIndex);
end;

procedure TDOSDropdown.SelectPrevious;
var
  StartIndex: Integer;
begin
  if Length(FCommands) = 0 then
    Exit;
    
  StartIndex := FSelectedIndex;
  repeat
    FSelectedIndex := FSelectedIndex - 1;
    if FSelectedIndex < 0 then
      FSelectedIndex := Length(FCommands) - 1;
  until (FCommands[FSelectedIndex].Enabled and not FCommands[FSelectedIndex].IsSeparator) or (FSelectedIndex = StartIndex);
end;

procedure TDOSDropdown.SelectFirst;
var
  I: Integer;
begin
  for I := 0 to Length(FCommands) - 1 do
  begin
    if FCommands[I].Enabled and not FCommands[I].IsSeparator then
    begin
      FSelectedIndex := I;
      Break;
    end;
  end;
end;

procedure TDOSDropdown.SelectLast;
var
  I: Integer;
begin
  for I := Length(FCommands) - 1 downto 0 do
  begin
    if FCommands[I].Enabled and not FCommands[I].IsSeparator then
    begin
      FSelectedIndex := I;
      Break;
    end;
  end;
end;

function TDOSDropdown.FindCommandByKey(Key: char): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FCommands) - 1 do
  begin
    if FCommands[I].MatchesKey(Key) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

// TDOSMenuItem implementation

constructor TDOSMenuItem.Create(const AText: string; AHotKey: char; AEnabled: Boolean);
begin
  inherited Create;
  FText := AText;
  FHotKey := AHotKey;
  FEnabled := AEnabled;
  FDropdown := nil;
  FHasDropdown := False;
end;

destructor TDOSMenuItem.Destroy;
begin
  if Assigned(FDropdown) then
    FDropdown.Free;
  inherited Destroy;
end;

procedure TDOSMenuItem.SetDropdown(ADropdown: TDOSDropdown);
begin
  if Assigned(FDropdown) then
    FDropdown.Free;
  FDropdown := ADropdown;
  FHasDropdown := Assigned(ADropdown);
end;

function TDOSMenuItem.MatchesKey(Key: char): Boolean;
begin
  Result := (FHotKey <> #0) and (UpCase(FHotKey) = UpCase(Key)) and FEnabled;
end;

// TDOSMenu implementation

constructor TDOSMenu.Create;
begin
  inherited Create;
  SetLength(FItems, 0);
  FSelectedIndex := 0;
  FVisible := True;
  FWidth := 80;
  FActiveDropdown := nil;
  FDropdownVisible := False;
  FDropdownPosition.X := 0;
  FDropdownPosition.Y := 0;
  FDropdownPosition.Width := 0;
  FDropdownPosition.Height := 0;
  FPendingAction := AnsiString('');
  FHasPendingAction := False;
end;

destructor TDOSMenu.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
    if Assigned(FItems[I]) then
      FItems[I].Free;
  SetLength(FItems, 0);
  inherited Destroy;
end;

procedure TDOSMenu.AddItem(const Text: string; Key: char; Enabled: Boolean);
var
  NewLength: Integer;
  Item: TDOSMenuItem;
begin
  Item := TDOSMenuItem.Create(Text, Key, Enabled);
  NewLength := Length(FItems);
  SetLength(FItems, NewLength + 1);
  FItems[NewLength] := Item;
end;

procedure TDOSMenu.AddItemWithDropdown(const Text: string; Key: char; ADropdown: TDOSDropdown; Enabled: Boolean);
var
  NewLength: Integer;
  Item: TDOSMenuItem;
begin
  Item := TDOSMenuItem.Create(Text, Key, Enabled);
  Item.SetDropdown(ADropdown);
  NewLength := Length(FItems);
  SetLength(FItems, NewLength + 1);
  FItems[NewLength] := Item;
end;

procedure TDOSMenu.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
    if Assigned(FItems[I]) then
      FItems[I].Free;
  SetLength(FItems, 0);
  FSelectedIndex := 0;
end;

function TDOSMenu.GetItemCount: Integer;
begin
  Result := Length(FItems);
end;

function TDOSMenu.GetSelectedItem: TDOSMenuItem;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FItems)) then
    Result := FItems[FSelectedIndex]
  else
    Result := nil;
end;

procedure TDOSMenu.ShowDropdown;
var
  MenuItem: TDOSMenuItem;
begin
  MenuItem := GetSelectedItem;
  if Assigned(MenuItem) and MenuItem.HasDropdown then
  begin
    FActiveDropdown := MenuItem.Dropdown;
    FDropdownVisible := True;
    FDropdownPosition := GetDropdownPosition;
  end;
end;

procedure TDOSMenu.HideDropdown;
begin
  FActiveDropdown := nil;
  FDropdownVisible := False;
end;

function TDOSMenu.HasActiveDropdown: Boolean;
begin
  Result := FDropdownVisible and Assigned(FActiveDropdown);
end;

function TDOSMenu.View: string;
var
  I: Integer;
  MenuItem: string;
  MenuLine: string;
  ItemLength: Integer;
  DebugFile: TextFile;
begin
  // Debug logging
  AssignFile(DebugFile, 'debug_menu.log');
  if FileExists('debug_menu.log') then
    Append(DebugFile)
  else
    Rewrite(DebugFile);
  
  WriteLn(DebugFile, '=== TDOSMenu.View START ===');
  WriteLn(DebugFile, 'FVisible: ', FVisible);
  WriteLn(DebugFile, 'Length(FItems): ', Length(FItems));
  WriteLn(DebugFile, 'FSelectedIndex: ', FSelectedIndex);
  WriteLn(DebugFile, 'FWidth: ', FWidth);
  
  if not FVisible or (Length(FItems) = 0) then
  begin
    WriteLn(DebugFile, 'EARLY EXIT: not visible or no items');
    WriteLn(DebugFile, '=== TDOSMenu.View END (empty) ===');
    CloseFile(DebugFile);
    Result := '';
    Exit;
  end;
  
  MenuLine := '';
  
  // Build the menu line with DOS-style formatting
  for I := 0 to Length(FItems) - 1 do
  begin
    WriteLn(DebugFile, 'Processing item ', I, ': ', FItems[I].Text);
    
    // Format menu item as " Text " with proper spacing
    MenuItem := ' ' + FItems[I].Text + ' ';
    
    if I = FSelectedIndex then
    begin
      // Selected item: white text on blue background (DOS style)
      MenuItem := GetRGBColorCode(RGB(255, 255, 255)) + GetRGBColorCode(RGB(0, 0, 170), True) + MenuItem + ResetColor;
      WriteLn(DebugFile, 'Item ', I, ' is SELECTED');
    end
    else if FItems[I].Enabled then
    begin
      // Enabled item: black text on light gray background (DOS style) 
      MenuItem := GetRGBColorCode(RGB(0, 0, 0)) + GetRGBColorCode(RGB(192, 192, 192), True) + MenuItem + ResetColor;
      WriteLn(DebugFile, 'Item ', I, ' is ENABLED');
    end
    else
    begin
      // Disabled item: dark gray text on light gray background
      MenuItem := GetRGBColorCode(RGB(128, 128, 128)) + GetRGBColorCode(RGB(192, 192, 192), True) + MenuItem + ResetColor;
      WriteLn(DebugFile, 'Item ', I, ' is DISABLED');
    end;
    
    MenuLine := MenuLine + MenuItem;
    WriteLn(DebugFile, 'MenuLine length so far: ', Length(MenuLine));
  end;
  
  // Pad the rest of the line with the background color
  ItemLength := Utf8DisplayWidth(StripAnsiEscapes(MenuLine));
  WriteLn(DebugFile, 'ItemLength before padding: ', ItemLength);
  while ItemLength < FWidth do
  begin
    MenuLine := MenuLine + GetRGBColorCode(RGB(192, 192, 192), True) + ' ' + ResetColor;
    Inc(ItemLength);
  end;
  
  WriteLn(DebugFile, 'Final MenuLine length: ', Length(MenuLine));
  WriteLn(DebugFile, 'Final ItemLength: ', ItemLength);
  WriteLn(DebugFile, '=== TDOSMenu.View END ===');
  CloseFile(DebugFile);
  
  Result := MenuLine;
end;

function TDOSMenu.Update(const Msg: TMsg): TDOSMenu;
var
  KeyMsg: TKeyMsg;
  NewMenu: TDOSMenu;
  I, FoundIndex: Integer;
  Item: TDOSMenuItem;
  SelectedCommand: TDOSMenuCommand;
  NewDropdown: TDOSDropdown;
  DebugFile: TextFile;
begin
  // Debug logging
  AssignFile(DebugFile, 'debug_menu_update.log');
  if FileExists('debug_menu_update.log') then
    Append(DebugFile)
  else
    Rewrite(DebugFile);
  
  WriteLn(DebugFile, '=== TDOSMenu.Update START ===');
  WriteLn(DebugFile, 'FVisible: ', FVisible);
  WriteLn(DebugFile, 'Length(FItems): ', Length(FItems));
  WriteLn(DebugFile, 'FSelectedIndex: ', FSelectedIndex);
  WriteLn(DebugFile, 'FDropdownVisible: ', FDropdownVisible);
  WriteLn(DebugFile, 'HasActiveDropdown: ', HasActiveDropdown);
  
  Result := Self; // Default: no change
  
  if not FVisible then
  begin
    WriteLn(DebugFile, 'EARLY EXIT: not visible');
    WriteLn(DebugFile, '=== TDOSMenu.Update END (not visible) ===');
    CloseFile(DebugFile);
    Exit;
  end;
  
  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    
    // Create new instance for immutable updates
    NewMenu := TDOSMenu.Create;
    NewMenu.FVisible := FVisible;
    NewMenu.FWidth := FWidth;
    NewMenu.FSelectedIndex := FSelectedIndex;
    NewMenu.FDropdownVisible := FDropdownVisible;
    NewMenu.FActiveDropdown := FActiveDropdown;
    NewMenu.FDropdownPosition := FDropdownPosition;
    NewMenu.FPendingAction := FPendingAction;
    NewMenu.FHasPendingAction := FHasPendingAction;
    
    // Copy items (deep copy to avoid shared references)
    SetLength(NewMenu.FItems, Length(FItems));
    WriteLn(DebugFile, 'Copying ', Length(FItems), ' items...');
    for I := 0 to Length(FItems) - 1 do
    begin
      // Create new menu item with same properties
      NewMenu.FItems[I] := TDOSMenuItem.Create(FItems[I].Text, FItems[I].HotKey, FItems[I].Enabled);
      
      // Deep copy dropdown if it exists
      if FItems[I].HasDropdown then
      begin
        WriteLn(DebugFile, '  Item ', I, ' has dropdown with ', FItems[I].Dropdown.GetCommandCount, ' commands');
        NewMenu.FItems[I].SetDropdown(CopyDropdown(FItems[I].Dropdown));
        WriteLn(DebugFile, '  Deep copied dropdown for item ', I);
      end;
      WriteLn(DebugFile, 'Deep copied item ', I, ': Text="', NewMenu.FItems[I].Text, '"');
    end;
    
    if KeyMsg.IsLeftArrow then
    begin
      NewMenu.SelectPrevious;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if KeyMsg.IsRightArrow then
    begin
      NewMenu.SelectNext;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if KeyMsg.Key = #9 then // Tab
    begin
      NewMenu.SelectNext;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if (KeyMsg.Key = #25) then // Shift+Tab (reverse tab)
    begin
      NewMenu.SelectPrevious;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if KeyMsg.Key = #1 then // Ctrl+A - Home
    begin
      NewMenu.SelectFirst;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if KeyMsg.Key = #5 then // Ctrl+E - End
    begin
      NewMenu.SelectLast;
      if NewMenu.HasActiveDropdown then
        NewMenu.HideDropdown;
      Result := NewMenu;
    end
    else if (KeyMsg.Key = #13) or (KeyMsg.Key = #10) then // Enter
    begin
      if NewMenu.HasActiveDropdown then
      begin
        // Execute selected dropdown command
        SelectedCommand := NewMenu.ActiveDropdown.GetSelectedCommand;
        if Assigned(SelectedCommand) and not SelectedCommand.IsSeparator then
        begin
          NewMenu.FPendingAction := SelectedCommand.Action;
          NewMenu.FHasPendingAction := True;
          NewMenu.HideDropdown;
          Result := NewMenu;
        end
        else
        begin
          // No valid command selected, just free the menu
          NewMenu.Free;
        end;
      end
      else
      begin
        // Show dropdown if current item has one
        Item := NewMenu.GetSelectedItem;
        if Assigned(Item) and Item.HasDropdown then
        begin
          NewMenu.ShowDropdown;
          Result := NewMenu;
        end
        else
        begin
          // No dropdown to show, free the menu
          NewMenu.Free;
        end;
      end;
    end
    else if KeyMsg.Key = #27 then // ESC - close dropdown
    begin
      if NewMenu.HasActiveDropdown then
      begin
        NewMenu.HideDropdown;
        Result := NewMenu;
      end
      else
      begin
        // No dropdown to close, free the menu
        NewMenu.Free;
      end;
    end
    else
    begin
      if NewMenu.HasActiveDropdown then
      begin
        // If dropdown is open, let it handle the message
        NewDropdown := NewMenu.ActiveDropdown.Update(Msg);
        if NewDropdown <> NewMenu.ActiveDropdown then
        begin
          NewMenu.FActiveDropdown := NewDropdown;
          Result := NewMenu;
        end
        else
        begin
          // Dropdown didn't handle the key, free the menu
          NewMenu.Free;
        end;
      end
      else
      begin
        // Check for hotkeys to select items and show dropdowns
        FoundIndex := NewMenu.FindItemByKey(KeyMsg.Key);
        WriteLn(DebugFile, 'FindItemByKey(', KeyMsg.Key, ') returned: ', FoundIndex);
        if FoundIndex >= 0 then
        begin
          NewMenu.FSelectedIndex := FoundIndex;
          Item := NewMenu.GetSelectedItem;
          WriteLn(DebugFile, 'Selected item ', FoundIndex, ': Text="', Item.Text, '"');
          WriteLn(DebugFile, 'Item has dropdown: ', Item.HasDropdown);
          if Assigned(Item) and Item.HasDropdown then
          begin
            WriteLn(DebugFile, 'Showing dropdown...');
            NewMenu.ShowDropdown;
            WriteLn(DebugFile, 'After ShowDropdown: ActiveDropdown assigned=', Assigned(NewMenu.FActiveDropdown));
            if Assigned(NewMenu.FActiveDropdown) then
              WriteLn(DebugFile, 'ActiveDropdown has ', NewMenu.FActiveDropdown.GetCommandCount, ' commands');
          end;
          Result := NewMenu;
        end
        else
        begin
          WriteLn(DebugFile, 'No matching item found, freeing NewMenu');
          // If we didn't find a match, free the new menu
          NewMenu.Free;
        end;
      end;
    end;
    
    // If we didn't create a new menu, free it
    if Result = Self then
    begin
      WriteLn(DebugFile, 'Freeing NewMenu (no change)');
      NewMenu.Free;
    end
    else
    begin
      WriteLn(DebugFile, 'Returning NewMenu with changes');
      WriteLn(DebugFile, 'NewMenu.FDropdownVisible: ', TDOSMenu(Result).FDropdownVisible);
      WriteLn(DebugFile, 'NewMenu.HasActiveDropdown: ', TDOSMenu(Result).HasActiveDropdown);
      WriteLn(DebugFile, 'NewMenu.HasPendingAction: ', TDOSMenu(Result).HasPendingAction);
    end;
  end
  else
  begin
    WriteLn(DebugFile, 'Message is not TKeyMsg');
  end;
  
  WriteLn(DebugFile, '=== TDOSMenu.Update END ===');
  CloseFile(DebugFile);
end;

procedure TDOSMenu.SelectNext;
var
  StartIndex: Integer;
begin
  if Length(FItems) = 0 then
    Exit;
    
  StartIndex := FSelectedIndex;
  repeat
    FSelectedIndex := (FSelectedIndex + 1) mod Length(FItems);
  until FItems[FSelectedIndex].Enabled or (FSelectedIndex = StartIndex);
end;

procedure TDOSMenu.SelectPrevious;
var
  StartIndex: Integer;
begin
  if Length(FItems) = 0 then
    Exit;
    
  StartIndex := FSelectedIndex;
  repeat
    FSelectedIndex := FSelectedIndex - 1;
    if FSelectedIndex < 0 then
      FSelectedIndex := Length(FItems) - 1;
  until FItems[FSelectedIndex].Enabled or (FSelectedIndex = StartIndex);
end;

procedure TDOSMenu.SelectFirst;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
  begin
    if FItems[I].Enabled then
    begin
      FSelectedIndex := I;
      Break;
    end;
  end;
end;

procedure TDOSMenu.SelectLast;
var
  I: Integer;
begin
  for I := Length(FItems) - 1 downto 0 do
  begin
    if FItems[I].Enabled then
    begin
      FSelectedIndex := I;
      Break;
    end;
  end;
end;

function TDOSMenu.GetSelectedItemPosition: Integer;
var
  I: Integer;
begin
  Result := 1; // Start at position 1 (1-based ANSI coordinates)
  
  // Calculate the X position of the selected menu item
  for I := 0 to FSelectedIndex - 1 do
  begin
    if I < Length(FItems) then
      Result := Result + Length(FItems[I].Text) + 2; // +2 for spaces around each item
  end;
end;

function TDOSMenu.GetDropdownPosition: TRect;
var
  XPos: Integer;
  MenuItem: TDOSMenuItem;
begin
  XPos := GetSelectedItemPosition;
  MenuItem := GetSelectedItem;
  
  if Assigned(MenuItem) and MenuItem.HasDropdown then
  begin
    Result.X := XPos;
    Result.Y := 2; // Below the menu bar
    Result.Width := MenuItem.Dropdown.Width;
    Result.Height := MenuItem.Dropdown.Height;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
    Result.Width := 0;
    Result.Height := 0;
  end;
end;

function TDOSMenu.FindItemByKey(Key: char): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FItems) - 1 do
  begin
    if FItems[I].MatchesKey(Key) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TDOSMenu.ShouldShowDropdown: Boolean;
begin
  Result := FDropdownVisible and Assigned(FActiveDropdown);
end;

function TDOSMenu.GetDropdownContent: string;
begin
  if ShouldShowDropdown then
    Result := FActiveDropdown.View
  else
    Result := '';
end;

function TDOSMenu.GetSelectedAction: string;
begin
  Result := FPendingAction;
end;

function TDOSMenu.HasPendingAction: Boolean;
begin
  Result := FHasPendingAction;
end;

procedure TDOSMenu.ClearPendingAction;
begin
  FPendingAction := AnsiString('');
  FHasPendingAction := False;
end;

function TDOSMenu.CopyDropdown(Source: TDOSDropdown): TDOSDropdown;
var
  I: Integer;
begin
  if not Assigned(Source) then
  begin
    Result := nil;
    Exit;
  end;
  
  // Create new dropdown with same properties
  Result := TDOSDropdown.Create(Source.Title);
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.SelectedIndex := Source.SelectedIndex;
  
  // Deep copy all commands
  for I := 0 to Source.GetCommandCount - 1 do
  begin
    if Source.FCommands[I].IsSeparator then
      Result.AddSeparator
    else
      Result.AddCommand(Source.FCommands[I].Text, Source.FCommands[I].HotKey, 
                       Source.FCommands[I].Enabled, Source.FCommands[I].Action);
  end;
end;

constructor TInputField.Create;
begin
  inherited Create;
  FText := AnsiString('');
  FPlaceholder := AnsiString('Enter text...');
  FWidth := 20;
  FFocused := false;
  FCursorPos := 0;
  FMaxLength := 100;
end;

function TInputField.GetDisplayText: string;
var
  DisplayText: string;
begin
  if Length(FText) = 0 then
  begin
    if FFocused then
      DisplayText := FPlaceholder
    else
      DisplayText := FPlaceholder;
  end
  else
    DisplayText := FText;
  
  // Ensure the display text fits within the width
  if Length(DisplayText) > FWidth - 2 then // Leave space for borders
    DisplayText := Copy(DisplayText, 1, FWidth - 2);
  
  // Add cursor if focused
  if FFocused then
  begin
    if FCursorPos <= Length(DisplayText) then
    begin
      // Insert cursor at current position
      if FCursorPos = 0 then
        DisplayText := AnsiString('█') + DisplayText
      else if FCursorPos >= Length(DisplayText) then
        DisplayText := DisplayText + AnsiString('█')
      else
      begin
        DisplayText := Copy(DisplayText, 1, FCursorPos) + AnsiString('█') + 
                      Copy(DisplayText, FCursorPos + 1, Length(DisplayText));
      end;
    end
    else
      DisplayText := DisplayText + AnsiString('█');
  end;
  
  // Pad to width
  while Length(DisplayText) < FWidth - 2 do
    DisplayText := DisplayText + AnsiString(' ');
  
  Result := DisplayText;
end;

function TInputField.View: string;
var
  Style: TStyle;
  DisplayText: string;
begin
  DisplayText := GetDisplayText;
  
  Style := TStyle.Create;
  try
    Style.BorderStyle := bsSingle;
    if FFocused then
      Style.BorderColor := cBrightBlue
    else
      Style.BorderColor := cDefault;
    Style.Width := FWidth;
    Style.Height := 3; // Single line with top and bottom borders
    Style.Content := DisplayText;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TInputField.Update(const Msg: TMsg): TInputField;
var
  KeyMsg: TKeyMsg;
  NewInput: TInputField;
begin
  Result := Self; // Default: no change
  
  if not FFocused then
    Exit; // Only handle input when focused
  
  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    
    // Create new instance for immutable updates
    NewInput := TInputField.Create;
    NewInput.FText := FText;
    NewInput.FPlaceholder := FPlaceholder;
    NewInput.FWidth := FWidth;
    NewInput.FFocused := FFocused;
    NewInput.FCursorPos := FCursorPos;
    NewInput.FMaxLength := FMaxLength;
    
    case KeyMsg.Key of
      #8, #127: // Backspace (different terminals send different codes)
        NewInput.Backspace;
      #1: // Ctrl+A - Home
        NewInput.MoveCursorHome;
      #5: // Ctrl+E - End
        NewInput.MoveCursorEnd;
      #2: // Ctrl+B - Left
        NewInput.MoveCursorLeft;
      #6: // Ctrl+F - Right
        NewInput.MoveCursorRight;
      #27: // ESC - might be arrow key sequence, but we'll keep it simple
        begin
          // For now, just ignore ESC
        end;
      else
      begin
        // Regular character input
        if (KeyMsg.Key >= ' ') and (KeyMsg.Key <= '~') then
          NewInput.InsertChar(KeyMsg.Key);
      end;
    end;
    
    Result := NewInput;
  end;
end;

procedure TInputField.Clear;
begin
  FText := AnsiString('');
  FCursorPos := 0;
end;

procedure TInputField.MoveCursorLeft;
begin
  if FCursorPos > 0 then
    Dec(FCursorPos);
end;

procedure TInputField.MoveCursorRight;
begin
  if FCursorPos < Length(FText) then
    Inc(FCursorPos);
end;

procedure TInputField.MoveCursorHome;
begin
  FCursorPos := 0;
end;

procedure TInputField.MoveCursorEnd;
begin
  FCursorPos := Length(FText);
end;

procedure TInputField.DeleteChar;
begin
  if FCursorPos < Length(FText) then
    FText := Copy(FText, 1, FCursorPos) + Copy(FText, FCursorPos + 2, Length(FText));
end;

procedure TInputField.Backspace;
begin
  if FCursorPos > 0 then
  begin
    FText := Copy(FText, 1, FCursorPos - 1) + Copy(FText, FCursorPos + 1, Length(FText));
    Dec(FCursorPos);
  end;
end;

procedure TInputField.InsertChar(Ch: char);
begin
  if Length(FText) < FMaxLength then
  begin
    FText := Copy(FText, 1, FCursorPos) + Ch + Copy(FText, FCursorPos + 1, Length(FText));
    Inc(FCursorPos);
  end;
end;

// Spinner implementation

constructor TSpinner.Create;
begin
  inherited Create;
  Inc(NextSpinnerId);
  FId := NextSpinnerId;
  FTag := 0;
  FFrame := 0;
  FSpinnerType := stLine;
  FStyle := nil;
  SetFramesForType(FSpinnerType);
end;

constructor TSpinner.Create(SpinnerType: TSpinnerType);
begin
  inherited Create;
  Inc(NextSpinnerId);
  FId := NextSpinnerId;
  FTag := 0;
  FFrame := 0;
  FSpinnerType := SpinnerType;
  FStyle := nil;
  SetFramesForType(FSpinnerType);
end;

destructor TSpinner.Destroy;
begin
  if Assigned(FStyle) then
    FStyle.Free;
  inherited Destroy;
end;

procedure TSpinner.SetFramesForType(SpinnerType: TSpinnerType);
begin
  SetLength(FFrames, 0);
  
  case SpinnerType of
    stLine:
      begin
        SetLength(FFrames, 4);
        FFrames[0] := '|';
        FFrames[1] := '/';
        FFrames[2] := '-';
        FFrames[3] := '\';
      end;
    stDot:
      begin
        SetLength(FFrames, 10);
        FFrames[0] := '⠋';
        FFrames[1] := '⠙';
        FFrames[2] := '⠹';
        FFrames[3] := '⠸';
        FFrames[4] := '⠼';
        FFrames[5] := '⠴';
        FFrames[6] := '⠦';
        FFrames[7] := '⠧';
        FFrames[8] := '⠇';
        FFrames[9] := '⠏';
      end;
    stMiniDot:
      begin
        SetLength(FFrames, 10);
        FFrames[0] := '⠁';
        FFrames[1] := '⠂';
        FFrames[2] := '⠄';
        FFrames[3] := '⡀';
        FFrames[4] := '⢀';
        FFrames[5] := '⠠';
        FFrames[6] := '⠐';
        FFrames[7] := '⠈';
        FFrames[8] := '⠁';
        FFrames[9] := '⠂';
      end;
    stJump:
      begin
        SetLength(FFrames, 6);
        FFrames[0] := '⢄';
        FFrames[1] := '⢂';
        FFrames[2] := '⢁';
        FFrames[3] := '⡁';
        FFrames[4] := '⡈';
        FFrames[5] := '⡐';
      end;
    stPulse:
      begin
        SetLength(FFrames, 9);
        FFrames[0] := '█';
        FFrames[1] := '▉';
        FFrames[2] := '▊';
        FFrames[3] := '▋';
        FFrames[4] := '▌';
        FFrames[5] := '▍';
        FFrames[6] := '▎';
        FFrames[7] := '▏';
        FFrames[8] := '▎';
      end;
    stPoints:
      begin
        SetLength(FFrames, 8);
        FFrames[0] := '∙∙∙';
        FFrames[1] := '●∙∙';
        FFrames[2] := '∙●∙';
        FFrames[3] := '∙∙●';
        FFrames[4] := '∙∙∙';
        FFrames[5] := '∙∙●';
        FFrames[6] := '∙●∙';
        FFrames[7] := '●∙∙';
      end;
    stGlobe:
      begin
        SetLength(FFrames, 6);
        FFrames[0] := '🌍';
        FFrames[1] := '🌎';
        FFrames[2] := '🌏';
        FFrames[3] := '🌎';
        FFrames[4] := '🌍';
        FFrames[5] := '🌏';
      end;
    stMoon:
      begin
        SetLength(FFrames, 8);
        FFrames[0] := '🌑';
        FFrames[1] := '🌒';
        FFrames[2] := '🌓';
        FFrames[3] := '🌔';
        FFrames[4] := '🌕';
        FFrames[5] := '🌖';
        FFrames[6] := '🌗';
        FFrames[7] := '🌘';
      end;
    stMonkey:
      begin
        SetLength(FFrames, 3);
        FFrames[0] := '🙈';
        FFrames[1] := '🙉';
        FFrames[2] := '🙊';
      end;
    stMeter:
      begin
        SetLength(FFrames, 7);
        FFrames[0] := '▁';
        FFrames[1] := '▃';
        FFrames[2] := '▄';
        FFrames[3] := '▅';
        FFrames[4] := '▆';
        FFrames[5] := '▇';
        FFrames[6] := '█';
      end;
    stHamburger:
      begin
        SetLength(FFrames, 3);
        FFrames[0] := '☰';
        FFrames[1] := '☱';
        FFrames[2] := '☲';
      end;
  end;
end;

function TSpinner.Update(const Msg: TMsg): TSpinner;
var
  ComponentTickMsg: TComponentTickMsg;
begin
  Result := Self;
  
  if Msg is TComponentTickMsg then
  begin
    ComponentTickMsg := TComponentTickMsg(Msg);
    
    // Only handle ticks for this specific spinner instance
    if ComponentTickMsg.ComponentId = 'spinner' + IntToStr(FId) then
    begin
      // Create a new spinner instance with updated frame
      Result := TSpinner.Create(FSpinnerType);
      Result.FId := FId;
      Result.FTag := FTag + 1;
      Result.FFrame := (FFrame + 1) mod Length(FFrames);
      
      // Copy style if present (simple copy for now)
      if Assigned(FStyle) then
      begin
        Result.FStyle := TStyle.Create;
        // Copy basic properties manually
        Result.FStyle.BorderStyle := FStyle.BorderStyle;
        Result.FStyle.BorderColor := FStyle.BorderColor;
        Result.FStyle.TitleColor := FStyle.TitleColor;
      end;
    end;
  end;
end;

function TSpinner.View: string;
var
  Frame: string;
begin
  if (FFrame >= 0) and (FFrame < Length(FFrames)) then
    Frame := FFrames[FFrame]
  else
    Frame := '';
    
  if Assigned(FStyle) then
  begin
    FStyle.Content := Frame;
    Result := FStyle.Render;
  end
  else
    Result := Frame;
end;

function TSpinner.Tick: TCmd;
begin
  // Return a command that will schedule this spinner's next tick
  Result := ComponentTickCmd('spinner' + IntToStr(FId), 100); // 100ms interval
end;

function TSpinner.WithStyle(AStyle: TStyle): TSpinner;
begin
  Result := Self;
  if Assigned(FStyle) then
    FStyle.Free;
  FStyle := AStyle;
end;

// TList implementation

constructor TList.Create;
begin
  inherited Create;
  SetLength(FItems, 0);
  FSelectedIndex := 0;
  FFocused := True;
  FWidth := 30;
  FHeight := 10;
  FTitle := AnsiString('');
  FSelectionIndicator := AnsiString('> ');
  FListId := AnsiString('');
  FHasPendingSelection := False;
  FPendingSelectionIndex := 0;
  FPendingSelectionItem := AnsiString('');
  FShowBorder := True;
  FBorderStyle := bsSingle;
  FBorderColor := cDefault;
  FSelectedColor := cBrightWhite;
  FScrollOffset := 0;
end;

destructor TList.Destroy;
begin
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TList.GetSelectedItem: string;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Length(FItems)) then
    Result := FItems[FSelectedIndex]
  else
    Result := AnsiString('');
end;

function TList.GetItemCount: integer;
begin
  Result := Length(FItems);
end;

function TList.GetVisibleItems: integer;
var
  ContentHeight: integer;
begin
  ContentHeight := FHeight;
  if FShowBorder then
    ContentHeight := ContentHeight - 2; // Top and bottom borders
  if FTitle <> '' then
    ContentHeight := ContentHeight - 2; // Title and spacing
  Result := ContentHeight;
end;

function TList.View: string;
var
  Style: TStyle;
  ContentLines: TStringArray;
  i, StartIdx, EndIdx: integer;
  ItemText: string;
  VisibleItems: integer;
begin
  SetLength(ContentLines, 0);
  
    // Calculate visible range with scrolling
    VisibleItems := GetVisibleItems;
    
    // Adjust scroll offset to keep selected item visible
    if FSelectedIndex < FScrollOffset then
      FScrollOffset := FSelectedIndex
    else if FSelectedIndex >= FScrollOffset + VisibleItems then
      FScrollOffset := FSelectedIndex - VisibleItems + 1;
    
    StartIdx := FScrollOffset;
    EndIdx := StartIdx + VisibleItems - 1;
    if EndIdx >= Length(FItems) then
      EndIdx := Length(FItems) - 1;
    
    // Build content
    for i := StartIdx to EndIdx do
    begin
      if i = FSelectedIndex then
      begin
        if FFocused and (FSelectedColor <> cDefault) then
          ItemText := bobastyle.GetColorCode(FSelectedColor) + FSelectionIndicator + FItems[i] + bobastyle.ResetColor
        else
          ItemText := FSelectionIndicator + FItems[i];
      end
      else
      begin
        // Add spacing to align with selection indicator
        ItemText := StringOfChar(' ', Length(FSelectionIndicator)) + FItems[i];
      end;
      SetLength(ContentLines, Length(ContentLines) + 1);
      ContentLines[High(ContentLines)] := ItemText;
    end;
    
    // Show scroll indicators if needed
    if (Length(FItems) > VisibleItems) and (Length(ContentLines) > 0) then
    begin
      if FScrollOffset > 0 then
        ContentLines[0] := ContentLines[0] + AnsiString(' ↑');
      if EndIdx < Length(FItems) - 1 then
        ContentLines[High(ContentLines)] := ContentLines[High(ContentLines)] + AnsiString(' ↓');
    end;
    
    if FShowBorder then
    begin
      Style := TStyle.Create;
      try
        Style.BorderStyle := FBorderStyle;
        if FFocused then
          Style.BorderColor := FBorderColor
        else
          Style.BorderColor := cDefault;
        Style.Width := FWidth;
        Style.Height := FHeight;
        Style.Content := bobastyle.JoinVertical(ContentLines);
        if FTitle <> '' then
        begin
          Style.Title := FTitle;
          Style.TitlePosition := tpCenter;
        end;
        Result := Style.Render;
      finally
        Style.Free;
      end;
    end
    else
    begin
      // No border, just the content
      if FTitle <> '' then
        Result := FTitle + #10 + #10 + bobastyle.JoinVertical(ContentLines)
      else
        Result := bobastyle.JoinVertical(ContentLines);
    end;
end;

function TList.Update(const Msg: bobaui.TMsg): TList;
var
  KeyMsg: bobaui.TKeyMsg;
  NewList: TList;
  DebugFile: TextFile;
begin
  Result := Self; // Default: no change
  
  if not FFocused then
    Exit;
  
  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    
    // Create new instance for immutable updates
    NewList := TList.Create;
    NewList.FItems := Copy(FItems);
    NewList.FSelectedIndex := FSelectedIndex;
    NewList.FFocused := FFocused;
    NewList.FWidth := FWidth;
    NewList.FHeight := FHeight;
    NewList.FTitle := FTitle;
    NewList.FSelectionIndicator := FSelectionIndicator;
    NewList.FListId := FListId;
    NewList.FHasPendingSelection := False;
    NewList.FPendingSelectionIndex := 0;
    NewList.FPendingSelectionItem := '';
    NewList.FShowBorder := FShowBorder;
    NewList.FBorderStyle := FBorderStyle;
    NewList.FBorderColor := FBorderColor;
    NewList.FSelectedColor := FSelectedColor;
    NewList.FScrollOffset := FScrollOffset;
    
    // Handle navigation
    if KeyMsg.IsUpArrow or (KeyMsg.Key = 'k') then
    begin
      NewList.SelectPrevious;
      Result := NewList;
    end
    else if KeyMsg.IsDownArrow or (KeyMsg.Key = 'j') then
    begin
      NewList.SelectNext;
      Result := NewList;
    end
    else if (KeyMsg.Key = #13) or (KeyMsg.Key = #10) then // Enter key (CR or LF)
    begin
      // Set pending selection only if list has items
      if Length(FItems) > 0 then
      begin
        NewList.FHasPendingSelection := True;
        NewList.FPendingSelectionIndex := FSelectedIndex;
        NewList.FPendingSelectionItem := GetSelectedItem;
      end;
      Result := NewList;
    end
    else if (KeyMsg.Key = 'g') then // Go to top
    begin
      NewList.SelectFirst;
      Result := NewList;
    end
    else if (KeyMsg.Key = 'G') then // Go to bottom
    begin
      NewList.SelectLast;
      Result := NewList;
    end
    else
    begin
      // No change, free the new list
      NewList.Free;
    end;
  end;
end;

procedure TList.AddItem(const Item: string);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := Item;
end;

procedure TList.ClearItems;
begin
  SetLength(FItems, 0);
  FSelectedIndex := 0;
  FScrollOffset := 0;
end;

procedure TList.SelectNext;
begin
  if Length(FItems) > 0 then
  begin
    FSelectedIndex := (FSelectedIndex + 1) mod Length(FItems);
  end;
end;

procedure TList.SelectPrevious;
begin
  if Length(FItems) > 0 then
  begin
    FSelectedIndex := FSelectedIndex - 1;
    if FSelectedIndex < 0 then
      FSelectedIndex := Length(FItems) - 1;
  end;
end;

procedure TList.SelectFirst;
begin
  FSelectedIndex := 0;
  FScrollOffset := 0;
end;

procedure TList.SelectLast;
begin
  if Length(FItems) > 0 then
    FSelectedIndex := Length(FItems) - 1;
end;

function TList.HasPendingSelection: Boolean;
begin
  Result := FHasPendingSelection;
end;

function TList.GetPendingSelection: TListSelectionMsg;
begin
  if FHasPendingSelection then
    Result := TListSelectionMsg.Create(FPendingSelectionIndex, FPendingSelectionItem, FListId)
  else
    Result := nil;
end;

procedure TList.ClearPendingSelection;
begin
  FHasPendingSelection := False;
  FPendingSelectionIndex := 0;
  FPendingSelectionItem := '';
end;

end.
