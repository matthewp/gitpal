unit bobaui;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  BaseUnix,
  TermIO,
  Unix,
  {$ENDIF}
  SysUtils,
  Classes,
  SyncObjs,
  bobastyle;

type
  TStringArray = array of string;

const
  STDIN_FILENO = 0;
  STDOUT_FILENO = 1;

var
  // Global reference to the program instance for signal handling and component ticks
  GlobalProgram: Pointer = nil;
  // Global state for component tick commands
  PendingComponentId: string = '';
  PendingComponentInterval: integer = 0;

type
  // Base message type
  TMsg = class
  public
    constructor Create;
  end;

  // Key press message
  TKeyMsg = class(TMsg)
  private
    FKey: char;
    FSequence: string;  // For escape sequences like arrow keys
  public
    constructor Create(const AKey: char); overload;
    constructor Create(const ASequence: string); overload;
    property Key: char read FKey;
    property Sequence: string read FSequence;
    function IsArrowKey: boolean;
    function IsUpArrow: boolean;
    function IsDownArrow: boolean;
    function IsLeftArrow: boolean;
    function IsRightArrow: boolean;
  end;

  // Quit message
  TQuitMsg = class(TMsg)
  public
    constructor Create;
  end;

  // Window resize message
  TWindowSizeMsg = class(TMsg)
  private
    FWidth, FHeight: integer;
  public
    constructor Create(const AWidth, AHeight: integer);
    property Width: integer read FWidth;
    property Height: integer read FHeight;
  end;

  // Timer tick message
  TTickMsg = class(TMsg)
  private
    FTime: TDateTime;
    FInterval: integer; // milliseconds
  public
    constructor Create(const ATime: TDateTime); overload;
    constructor Create(const ATime: TDateTime; const AInterval: integer); overload;
    property Time: TDateTime read FTime;
    property Interval: integer read FInterval;
  end;


  // Cursor control messages
  TShowCursorMsg = class(TMsg)
  public
    constructor Create;
  end;

  THideCursorMsg = class(TMsg)
  public
    constructor Create;
  end;

  // Component-specific tick message for self-scheduling animations
  TComponentTickMsg = class(TMsg)
  private
    FComponentId: string;
    FInterval: integer;
  public
    constructor Create(const AComponentId: string; AInterval: integer);
    property ComponentId: string read FComponentId;
    property Interval: integer read FInterval;
  end;


  // Command type (function that returns a message)
  TCmd = function: TMsg; cdecl;

  // Message queue for thread-safe communication
  TMessageQueue = class
  private
    FInputMsgObjects: array of TMsg;  // High priority queue for input messages
    FInputCount: integer;
    FAnimationMsgObjects: array of TMsg;  // Lower priority queue for animation messages
    FAnimationCount: integer;
    FCS: TCriticalSection;
    FDestroying: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(const Msg: TMsg);
    function Pop: TMsg;
    function IsEmpty: Boolean;
    function Count: integer;
    procedure Clear;
    procedure Shutdown;
  end;

  // Display mode enumeration
  TDisplayMode = (dmInline, dmFullScreen);

  // Rectangle type for overlay positioning
  TRect = record
    X, Y: Integer;        // Top-left position (1-based, like ANSI)
    Width, Height: Integer;  // Dimensions
  end;

  // Forward declarations
  TModel = class;
  TOverlay = class;
  
  TUpdateResult = record
    Model: TModel;
    Cmd: TCmd;
  end;


  // Abstract display class that defines our display interface
  TDisplay = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    
    // Core display operations
    procedure Clear; virtual; abstract;
    procedure Write(const S: string); virtual; abstract;
    procedure WriteLn(const S: string); virtual; abstract;
    procedure MoveCursor(X, Y: integer); virtual; abstract;
    
    // Input operations
    function ReadKey: char; virtual; abstract;
    function KeyPressed: boolean; virtual; abstract;
    
    // Screen dimensions
    function GetWidth: integer; virtual; abstract;
    function GetHeight: integer; virtual; abstract;
  end;

  // ANSI terminal implementation (UTF-8 safe, no CRT dependency)
  TAnsiDisplay = class(TDisplay)
  private
    FTerminalWidth, FTerminalHeight: integer;
    FLastRender: TStringArray;
    FNeedsFullRedraw: Boolean;
    FDisplayMode: TDisplayMode;
    FInlineBaseRow, FInlineBaseCol: Integer; // Starting position for inline mode
    FInlineInitialized: Boolean;
    {$IFDEF UNIX}
    FOriginalTermios: TermIOs;
    {$ENDIF}
    procedure DetectTerminalSize;
    procedure WriteAnsiSequence(const Sequence: string);
    procedure SetRawMode;
    procedure RestoreTerminal;
    procedure DifferentialWrite(const Content: string);
    procedure ClearToEndOfLine;
    procedure ClearFromCursor;
  public
    constructor Create(AMode: TDisplayMode = dmInline); overload;
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Write(const S: string); override;
    procedure WriteLn(const S: string); override;
    procedure MoveCursor(X, Y: integer); override;
    function ReadKey: char; override;
    function KeyPressed: boolean; override;
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    procedure ForceFullRedraw;
    function GetDisplayMode: TDisplayMode;
  end;

  // Base model type that all applications will extend
  TModel = class
  private
    FOverlays: array of TOverlay;
    procedure SortOverlaysByZIndex;
    function CompositeOverlay(var BaseLines: TStringArray; const Overlay: TOverlay): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function View: string; virtual; abstract;
    function Update(const Msg: TMsg): TUpdateResult; virtual; abstract;
    
    // Overlay management
    procedure AddOverlay(Overlay: TOverlay);
    procedure RemoveOverlay(Index: Integer);
    procedure ClearOverlays;
    function RenderWithOverlays: string; virtual;
    function HasOverlays: Boolean;
    
    // Helper for event routing to overlays
    function UpdateOverlays(const Msg: TMsg): TUpdateResult;
  end;

  // Overlay class for modal dialogs, menus, tooltips, etc.
  TOverlay = class
  private
    FZIndex: Integer;
    FContent: TModel;
    FPosition: TRect;
    FVisible: Boolean;
  public
    constructor Create(AContent: TModel; APosition: TRect; AZIndex: Integer = 100);
    destructor Destroy; override;
    property ZIndex: Integer read FZIndex write FZIndex;
    property Content: TModel read FContent write FContent;
    property Position: TRect read FPosition write FPosition;
    property Visible: Boolean read FVisible write FVisible;
  end;

  // Full overlay message declarations (after TModel)
  TAddOverlayMsg = class(TMsg)
  private
    FContent: TModel;
    FPosition: TRect;
    FZIndex: Integer;
  public
    constructor Create(AContent: TModel; APosition: TRect; AZIndex: Integer);
    property Content: TModel read FContent write FContent;
    property Position: TRect read FPosition write FPosition;
    property ZIndex: Integer read FZIndex write FZIndex;
  end;

  TRemoveTopOverlayMsg = class(TMsg)
  public
    constructor Create;
  end;

  TClearOverlaysMsg = class(TMsg)
  public
    constructor Create;
  end;

  // Input thread for non-blocking keyboard input
  TInputThread = class(TThread)
  private
    FMessageQueue: TMessageQueue;
    FDisplay: TDisplay;
  protected
    procedure Execute; override;
  public
    constructor Create(AMessageQueue: TMessageQueue; ADisplay: TDisplay);
  end;

  // Component tick info
  TComponentTick = record
    ComponentId: string;
    Interval: integer;
    NextTick: TDateTime;
  end;

  // Timer thread for component tick updates
  TTimerThread = class(TThread)
  private
    FMessageQueue: TMessageQueue;
    FComponentTicks: array of TComponentTick;
    FComponentTickCS: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(AMessageQueue: TMessageQueue);
    destructor Destroy; override;
    procedure ScheduleComponentTick(const ComponentId: string; Interval: integer);
  end;

  TBobaUIProgram = class
  private
    FModel: TModel;
    FRunning: boolean;
    FDisplay: TDisplay;
    FMessageQueue: TMessageQueue;
    FInputThread: TInputThread;
    FTimerThread: TTimerThread;
    {$IFDEF UNIX}
    FOriginalSigWinchHandler: PSigActionRec;
    {$ENDIF}
    procedure InitializeSignalHandlers;
    procedure RestoreSignalHandlers;
    procedure HandleWindowResize(Width, Height: integer);
  public
    constructor Create(AModel: TModel; ADisplay: TDisplay); overload;
    constructor Create(AModel: TModel; ADisplayMode: TDisplayMode = dmInline); overload;
    destructor Destroy; override;
    procedure Run;
    procedure Quit;
    procedure ScheduleComponentTick(const ComponentId: string; Interval: integer);
  end;

// Common commands
function QuitCmd: TCmd;
function ShowCursorCmd: TCmd;
function HideCursorCmd: TCmd;

// Animation commands
function TickCmd(Interval: integer): TCmd; // Create a timer that ticks every Interval milliseconds
function ComponentTickCmd(const ComponentId: string; Interval: integer): TCmd; // Create component-specific tick

// Overlay commands
function AddOverlayCmd(Content: TModel; Position: TRect; ZIndex: Integer = 100): TCmd;
function RemoveTopOverlayCmd: TCmd;
function ClearOverlaysCmd: TCmd;

// Helper functions for creating common overlay positions
function CenterRect(Width, Height: Integer): TRect;
function CenterRectInBounds(Width, Height, ScreenWidth, ScreenHeight: Integer): TRect;
function CreateRect(X, Y, Width, Height: Integer): TRect;
function PositionRectBelowMenu(MenuItemX, DropdownWidth, DropdownHeight, ScreenWidth, ScreenHeight: Integer): TRect;

// Helper functions for overlay rendering
procedure ExtractActiveColors(const Text: string; out BackgroundColor, ForegroundColor: string);
function ExtractActiveBackgroundColor(const Text: string): string;
function StripAllResets(const Text: string): string;

// Debug logging
procedure DebugLog(const Message: string);

// Standalone functions for commands
function DoQuitCmd: TMsg; cdecl;
function DoShowCursorCmd: TMsg; cdecl;
function DoHideCursorCmd: TMsg; cdecl;
function DoTickCmd(Interval: integer): TMsg; cdecl;
function DoComponentTickCmd: TMsg; cdecl;

// Overlay command functions
function DoAddOverlayCmd: TMsg; cdecl;
function DoRemoveTopOverlayCmd: TMsg; cdecl;
function DoClearOverlaysCmd: TMsg; cdecl;

{$IFDEF UNIX}
// Signal handler for SIGWINCH (terminal resize)
procedure HandleSIGWINCH(sig: cint); cdecl;

// Function to get current terminal size
procedure GetCurrentTerminalSize(out Width, Height: integer);
{$ENDIF}

implementation

constructor TMsg.Create;
begin
  inherited Create;
end;

constructor TKeyMsg.Create(const AKey: char);
begin
  inherited Create;
  FKey := AKey;
  FSequence := '';
end;

constructor TKeyMsg.Create(const ASequence: string);
begin
  inherited Create;
  FSequence := ASequence;
  FKey := #0;  // No single key for sequences
end;

function TKeyMsg.IsArrowKey: boolean;
begin
  Result := (FSequence = #27'[A') or (FSequence = #27'[B') or 
           (FSequence = #27'[C') or (FSequence = #27'[D');
end;

function TKeyMsg.IsUpArrow: boolean;
begin
  Result := FSequence = #27'[A';
end;

function TKeyMsg.IsDownArrow: boolean;
begin
  Result := FSequence = #27'[B';
end;

function TKeyMsg.IsRightArrow: boolean;
begin
  Result := FSequence = #27'[C';
end;

function TKeyMsg.IsLeftArrow: boolean;
begin
  Result := FSequence = #27'[D';
end;

constructor TQuitMsg.Create;
begin
  inherited Create;
end;

constructor TWindowSizeMsg.Create(const AWidth, AHeight: integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
end;

constructor TTickMsg.Create(const ATime: TDateTime);
begin
  inherited Create;
  FTime := ATime;
  FInterval := 1000; // Default 1 second
end;

constructor TTickMsg.Create(const ATime: TDateTime; const AInterval: integer);
begin
  inherited Create;
  FTime := ATime;
  FInterval := AInterval;
end;


constructor TShowCursorMsg.Create;
begin
  inherited Create;
end;

constructor THideCursorMsg.Create;
begin
  inherited Create;
end;

constructor TComponentTickMsg.Create(const AComponentId: string; AInterval: integer);
begin
  inherited Create;
  FComponentId := AComponentId;
  FInterval := AInterval;
end;

constructor TAddOverlayMsg.Create(AContent: TModel; APosition: TRect; AZIndex: Integer);
begin
  inherited Create;
  FContent := AContent;
  FPosition := APosition;
  FZIndex := AZIndex;
end;

constructor TRemoveTopOverlayMsg.Create;
begin
  inherited Create;
end;

constructor TClearOverlaysMsg.Create;
begin
  inherited Create;
end;

constructor TOverlay.Create(AContent: TModel; APosition: TRect; AZIndex: Integer);
begin
  inherited Create;
  FContent := AContent;
  FPosition := APosition;
  FZIndex := AZIndex;
  FVisible := True;
end;

destructor TOverlay.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited Destroy;
end;

constructor TMessageQueue.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FInputCount := 0;
  FAnimationCount := 0;
  FDestroying := False;
  SetLength(FInputMsgObjects, 0);
  SetLength(FAnimationMsgObjects, 0);
end;

destructor TMessageQueue.Destroy;
begin
  FCS.Enter;
  try
    FDestroying := True;
    Clear;
  finally
    FCS.Leave;
  end;
  FCS.Free;
  inherited Destroy;
end;

procedure TMessageQueue.Push(const Msg: TMsg);
var
  i: integer;
begin
  FCS.Enter;
  try
    if FDestroying then
    begin
      // If we're destroying, just free the message immediately
      if Assigned(Msg) then
        Msg.Free;
      Exit;
    end;
    
    // Prioritize input messages over animation messages
    if (Msg is TKeyMsg) or (Msg is TWindowSizeMsg) or (Msg is TQuitMsg) then
    begin
      // High priority input message
      if FInputCount >= Length(FInputMsgObjects) then
        SetLength(FInputMsgObjects, FInputCount + 10);
      FInputMsgObjects[FInputCount] := Msg;
      Inc(FInputCount);
    end
    else
    begin
      // Lower priority animation/timer message
      // Limit animation queue size to prevent buildup that causes lag
      if FAnimationCount >= 5 then
      begin
        // Drop oldest animation message to prevent queue buildup
        if Assigned(FAnimationMsgObjects[0]) then
          FAnimationMsgObjects[0].Free;
        // Shift remaining messages
        for i := 1 to FAnimationCount - 1 do
          FAnimationMsgObjects[i - 1] := FAnimationMsgObjects[i];
        Dec(FAnimationCount);
      end;
      
      if FAnimationCount >= Length(FAnimationMsgObjects) then
        SetLength(FAnimationMsgObjects, FAnimationCount + 10);
      FAnimationMsgObjects[FAnimationCount] := Msg;
      Inc(FAnimationCount);
    end;
  finally
    FCS.Leave;
  end;
end;

function TMessageQueue.Pop: TMsg;
var
  i: integer;
begin
  Result := nil;
  FCS.Enter;
  try
    if FDestroying then
      Exit;
    
    // Always prioritize input messages first
    if FInputCount > 0 then
    begin
      Result := FInputMsgObjects[0];
      // Shift remaining input messages
      for i := 1 to FInputCount - 1 do
        FInputMsgObjects[i - 1] := FInputMsgObjects[i];
      Dec(FInputCount);
    end
    else if FAnimationCount > 0 then
    begin
      Result := FAnimationMsgObjects[0];
      // Shift remaining animation messages
      for i := 1 to FAnimationCount - 1 do
        FAnimationMsgObjects[i - 1] := FAnimationMsgObjects[i];
      Dec(FAnimationCount);
    end;
  finally
    FCS.Leave;
  end;
end;

function TMessageQueue.IsEmpty: Boolean;
begin
  FCS.Enter;
  try
    Result := (FInputCount = 0) and (FAnimationCount = 0);
  finally
    FCS.Leave;
  end;
end;

function TMessageQueue.Count: integer;
begin
  FCS.Enter;
  try
    Result := FInputCount + FAnimationCount;
  finally
    FCS.Leave;
  end;
end;

procedure TMessageQueue.Clear;
var
  i: integer;
begin
  // Note: Caller must hold FCS lock
  for i := 0 to FInputCount - 1 do
    if Assigned(FInputMsgObjects[i]) then
      FInputMsgObjects[i].Free;
  for i := 0 to FAnimationCount - 1 do
    if Assigned(FAnimationMsgObjects[i]) then
      FAnimationMsgObjects[i].Free;
  SetLength(FInputMsgObjects, 0);
  SetLength(FAnimationMsgObjects, 0);
  FInputCount := 0;
  FAnimationCount := 0;
end;

procedure TMessageQueue.Shutdown;
begin
  FCS.Enter;
  try
    FDestroying := True;
    Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TModel.Create;
begin
  inherited Create;
  SetLength(FOverlays, 0);
end;

destructor TModel.Destroy;
begin
  ClearOverlays;
  inherited Destroy;
end;

procedure TModel.AddOverlay(Overlay: TOverlay);
var
  NewLength: Integer;
begin
  NewLength := Length(FOverlays);
  SetLength(FOverlays, NewLength + 1);
  FOverlays[NewLength] := Overlay;
end;

procedure TModel.RemoveOverlay(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < Length(FOverlays)) then
  begin
    if Assigned(FOverlays[Index]) then
      FOverlays[Index].Free;
    
    // Shift remaining overlays down
    for I := Index to Length(FOverlays) - 2 do
      FOverlays[I] := FOverlays[I + 1];
    
    SetLength(FOverlays, Length(FOverlays) - 1);
  end;
end;

procedure TModel.ClearOverlays;
var
  I: Integer;
begin
  for I := 0 to Length(FOverlays) - 1 do
  begin
    if Assigned(FOverlays[I]) then
      FOverlays[I].Free;
  end;
  SetLength(FOverlays, 0);
end;

function TModel.HasOverlays: Boolean;
begin
  Result := Length(FOverlays) > 0;
end;

procedure TModel.SortOverlaysByZIndex;
var
  I, J: Integer;
  Temp: TOverlay;
begin
  // Simple bubble sort by z-index (low to high)
  for I := 0 to Length(FOverlays) - 2 do
  begin
    for J := 0 to Length(FOverlays) - 2 - I do
    begin
      if FOverlays[J].ZIndex > FOverlays[J + 1].ZIndex then
      begin
        Temp := FOverlays[J];
        FOverlays[J] := FOverlays[J + 1];
        FOverlays[J + 1] := Temp;
      end;
    end;
  end;
end;

// Helper function to extract both active background and foreground colors from text
procedure ExtractActiveColors(const Text: string; out BackgroundColor, ForegroundColor: string);
var
  I: Integer;
  EscapeSeq: string;
begin
  BackgroundColor := '';
  ForegroundColor := '';
  I := 1;
  
  // Find the FIRST background and foreground colors in the text
  // This is because base content typically starts with colors and ends with reset
  while I <= Length(Text) do
  begin
    if (I <= Length(Text) - 1) and (Text[I] = #27) and (Text[I + 1] = '[') then
    begin
      // Found start of escape sequence
      I := I + 2; // Skip ESC[
      
      // Read until 'm'
      EscapeSeq := #27'[';
      while (I <= Length(Text)) and (Text[I] <> 'm') do
      begin
        EscapeSeq := EscapeSeq + Text[I];
        Inc(I);
      end;
      
      if (I <= Length(Text)) and (Text[I] = 'm') then
      begin
        EscapeSeq := EscapeSeq + 'm';
        
        // Check if this is a background color escape
        // Background colors: 40-47 (standard), 100-107 (bright), 48;5;n (256), 48;2;r;g;b (RGB)
        if (Pos('[40m', EscapeSeq) > 0) or (Pos('[41m', EscapeSeq) > 0) or (Pos('[42m', EscapeSeq) > 0) or 
           (Pos('[43m', EscapeSeq) > 0) or (Pos('[44m', EscapeSeq) > 0) or (Pos('[45m', EscapeSeq) > 0) or
           (Pos('[46m', EscapeSeq) > 0) or (Pos('[47m', EscapeSeq) > 0) or (Pos('[100m', EscapeSeq) > 0) or
           (Pos('[101m', EscapeSeq) > 0) or (Pos('[102m', EscapeSeq) > 0) or (Pos('[103m', EscapeSeq) > 0) or
           (Pos('[104m', EscapeSeq) > 0) or (Pos('[105m', EscapeSeq) > 0) or (Pos('[106m', EscapeSeq) > 0) or
           (Pos('[107m', EscapeSeq) > 0) or (Pos('[48;5;', EscapeSeq) > 0) or (Pos('[48;2;', EscapeSeq) > 0) then
        begin
          // Found a background color - if this is the first one, use it
          if Length(BackgroundColor) = 0 then
          begin
            BackgroundColor := EscapeSeq;
          end;
        end
        // Check if this is a foreground color escape  
        // Foreground colors: 30-37 (standard), 90-97 (bright), 38;5;n (256), 38;2;r;g;b (RGB)
        else if (Pos('[30m', EscapeSeq) > 0) or (Pos('[31m', EscapeSeq) > 0) or (Pos('[32m', EscapeSeq) > 0) or
                (Pos('[33m', EscapeSeq) > 0) or (Pos('[34m', EscapeSeq) > 0) or (Pos('[35m', EscapeSeq) > 0) or
                (Pos('[36m', EscapeSeq) > 0) or (Pos('[37m', EscapeSeq) > 0) or (Pos('[90m', EscapeSeq) > 0) or
                (Pos('[91m', EscapeSeq) > 0) or (Pos('[92m', EscapeSeq) > 0) or (Pos('[93m', EscapeSeq) > 0) or
                (Pos('[94m', EscapeSeq) > 0) or (Pos('[95m', EscapeSeq) > 0) or (Pos('[96m', EscapeSeq) > 0) or
                (Pos('[97m', EscapeSeq) > 0) or (Pos('[38;5;', EscapeSeq) > 0) or (Pos('[38;2;', EscapeSeq) > 0) then
        begin
          // Found a foreground color - if this is the first one, use it
          if Length(ForegroundColor) = 0 then
          begin
            ForegroundColor := EscapeSeq;
          end;
        end;
        
        // If we found both colors, we can exit early
        if (Length(BackgroundColor) > 0) and (Length(ForegroundColor) > 0) then
          Exit;
      end;
      Inc(I);
    end
    else
      Inc(I);
  end;
end;

// Helper function to extract the active background color from text (kept for backward compatibility)
function ExtractActiveBackgroundColor(const Text: string): string;
var
  ForegroundColor: string;
begin
  ExtractActiveColors(Text, Result, ForegroundColor);
end;

// Helper function to strip ALL ANSI reset sequences from overlay content
function StripAllResets(const Text: string): string;
var
  I, Start: Integer;
  EscapeSeq: string;
begin
  Result := Text;
  if Length(Result) = 0 then
    Exit;
    
  I := 1;
  while I <= Length(Result) do
  begin
    if (I <= Length(Result) - 1) and (Result[I] = #27) and (Result[I + 1] = '[') then
    begin
      // Found start of escape sequence
      Start := I;
      I := I + 2; // Skip ESC[
      
      // Read until 'm'
      EscapeSeq := #27'[';
      while (I <= Length(Result)) and (Result[I] <> 'm') do
      begin
        EscapeSeq := EscapeSeq + Result[I];
        Inc(I);
      end;
      
      if (I <= Length(Result)) and (Result[I] = 'm') then
      begin
        EscapeSeq := EscapeSeq + 'm';
        
        // Check if this is a reset sequence
        if (EscapeSeq = #27'[0m') or (EscapeSeq = #27'[m') or
           (EscapeSeq = #27'[39m') or (EscapeSeq = #27'[49m') or
           (EscapeSeq = #27'[39;49m') or (EscapeSeq = #27'[49;39m') then
        begin
          // Remove this reset sequence
          Delete(Result, Start, Length(EscapeSeq));
          // Adjust I since we removed characters
          I := Start;
        end
        else
        begin
          // Not a reset sequence, move past it
          Inc(I);
        end;
      end
      else
      begin
        // No 'm' found, move past this character
        I := Start + 1;
      end;
    end
    else
      Inc(I);
  end;
end;

// Debug logging procedure
procedure DebugLog(const Message: string);
var
  LogFile: TextFile;
begin
  try
    AssignFile(LogFile, 'debug.log');
    if FileExists('debug.log') then
      Append(LogFile)
    else
      Rewrite(LogFile);
    WriteLn(LogFile, FormatDateTime('hh:nn:ss.zzz', Now), ' - ', Message);
    CloseFile(LogFile);
  except
    // Silently ignore logging errors
  end;
end;

function TModel.CompositeOverlay(var BaseLines: TStringArray; const Overlay: TOverlay): Boolean;
var
  OverlayLines: TStringArray;
  I: Integer;
  OverlayX, OverlayY: Integer;
  TargetY: Integer;
  OverlayLine: string;
  BaseLine: string;
  BeforePart, AfterPart: string;
  DummyPart: string;
  TempAfterPart: string;
  ActiveBgColor: string;
  ActiveFgColor: string;
begin
  Result := False;
  if not Assigned(Overlay) or not Overlay.Visible or not Assigned(Overlay.Content) then
    Exit;

  try
    OverlayLines := bobastyle.SplitLines(Overlay.Content.View);
    OverlayX := Overlay.Position.X;
    OverlayY := Overlay.Position.Y;
    

    // Composite each line of the overlay onto the base
    for I := 0 to Length(OverlayLines) - 1 do
    begin
      TargetY := OverlayY + I;
      
      // Skip if overlay line is outside base content bounds
      if (TargetY < 1) or (TargetY > Length(BaseLines)) then
        Continue;
      
      OverlayLine := OverlayLines[I];
      if Length(OverlayLine) = 0 then
        Continue;
      
      // Strip ALL reset sequences to prevent background color bleeding
      OverlayLine := StripAllResets(OverlayLine);
      
      // Adjust for 0-based array indexing
      BaseLine := BaseLines[TargetY - 1];
      
      // Extract both background and foreground colors from original base line BEFORE any modifications
      ExtractActiveColors(BaseLine, ActiveBgColor, ActiveFgColor);
      
      // If overlay starts beyond the base line, extend the base line
      while Utf8DisplayWidth(BaseLine) < OverlayX - 1 do
        BaseLine := BaseLine + ' ';
      
      // Split base line into before and after parts using display-aware splitting
      if not SplitAnsiStringAtDisplay(BaseLine, OverlayX - 1, BeforePart, AfterPart) then
      begin
        // Fallback if split failed
        BeforePart := BaseLine;
        AfterPart := '';
      end
      else
      begin
        
        // Now split the after part to account for the overlay width
        if Utf8DisplayWidth(OverlayLine) > 0 then
        begin
          // We need to skip the part that will be replaced by the overlay
          // and keep everything after that
          TempAfterPart := AfterPart;
          if not SplitAnsiStringAtDisplay(TempAfterPart, Utf8DisplayWidth(OverlayLine), DummyPart, AfterPart) then
          begin
            // If split failed, keep the entire AfterPart
            // (don't set it to empty string)
          end;
          // If split succeeded, DummyPart contains the replaced text (discarded)
          // and AfterPart contains the text after the overlay
        end;
      end;
      
      // Restore the original background and foreground colors after the overlay
      if Length(AfterPart) > 0 then
      begin
        // Apply both background and foreground colors if available
        if (Length(ActiveBgColor) > 0) and (Length(ActiveFgColor) > 0) then
          AfterPart := ActiveBgColor + ActiveFgColor + AfterPart
        else if Length(ActiveBgColor) > 0 then
          AfterPart := ActiveBgColor + AfterPart
        else if Length(ActiveFgColor) > 0 then
          AfterPart := ActiveFgColor + AfterPart;
      end;
      
      // Combine the parts
      BaseLines[TargetY - 1] := BeforePart + OverlayLine + AfterPart;
    end;
    
    Result := True;
  except
    // Silently handle any compositing errors
    Result := False;
  end;
end;

function TModel.RenderWithOverlays: string;
var
  BaseLines: TStringArray;
  I: Integer;
begin
  // Get base content as lines
  BaseLines := bobastyle.SplitLines(View);
  
  if Length(FOverlays) = 0 then
  begin
    Result := View;
    Exit;
  end;
  
  // Sort overlays by z-index (low to high for rendering order)
  SortOverlaysByZIndex;
  
  // Composite each overlay onto the base
  for I := 0 to Length(FOverlays) - 1 do
  begin
    CompositeOverlay(BaseLines, FOverlays[I]);
  end;
  
  Result := bobastyle.JoinVertical(BaseLines);
end;

function TModel.UpdateOverlays(const Msg: TMsg): TUpdateResult;
var
  I: Integer;
  OverlayResult: TUpdateResult;
begin
  Result.Model := Self;
  Result.Cmd := nil;
  
  // Handle overlay management messages first
  if Msg is TAddOverlayMsg then
  begin
    AddOverlay(TOverlay.Create(
      TAddOverlayMsg(Msg).Content,
      TAddOverlayMsg(Msg).Position,
      TAddOverlayMsg(Msg).ZIndex
    ));
    Exit;
  end
  else if Msg is TRemoveTopOverlayMsg then
  begin
    if Length(FOverlays) > 0 then
      RemoveOverlay(Length(FOverlays) - 1);
    Exit;
  end
  else if Msg is TClearOverlaysMsg then
  begin
    ClearOverlays;
    Exit;
  end;
  
  // Try to route message to overlays (highest z-index first)
  SortOverlaysByZIndex;
  for I := Length(FOverlays) - 1 downto 0 do
  begin
    if FOverlays[I].Visible and Assigned(FOverlays[I].Content) then
    begin
      OverlayResult := FOverlays[I].Content.Update(Msg);
      if OverlayResult.Model <> FOverlays[I].Content then
      begin
        // Overlay state changed, update it
        FOverlays[I].Content.Free;
        FOverlays[I].Content := OverlayResult.Model;
        Result.Cmd := OverlayResult.Cmd;
        Exit; // Message consumed by overlay
      end;
    end;
  end;
end;

constructor TDisplay.Create;
begin
  inherited Create;
end;

destructor TDisplay.Destroy;
begin
  inherited Destroy;
end;

constructor TAnsiDisplay.Create(AMode: TDisplayMode);
begin
  inherited Create;
  FDisplayMode := AMode;
  DetectTerminalSize;
  SetRawMode;
  
  // Initialize differential rendering
  SetLength(FLastRender, 0);
  FNeedsFullRedraw := True;
  
  // Initialize inline mode tracking
  FInlineInitialized := False;
  FInlineBaseRow := 1;
  FInlineBaseCol := 1;
  
  // Only use alternate screen for full screen mode
  if FDisplayMode = dmFullScreen then
  begin
    // Switch to alternate screen buffer
    WriteAnsiSequence(#27'[?1049h');
    // Hide cursor by default (like BubbleTea)
    WriteAnsiSequence(#27'[?25l');
  end;
  // For inline mode (default), preserve existing terminal content
end;

constructor TAnsiDisplay.Create;
begin
  Create(dmInline);
end;

destructor TAnsiDisplay.Destroy;
begin
  if FDisplayMode = dmFullScreen then
  begin
    // Show cursor on exit
    WriteAnsiSequence(#27'[?25h');
    // Switch back to main screen buffer
    WriteAnsiSequence(#27'[?1049l');
  end;
  RestoreTerminal;
  inherited Destroy;
end;

procedure TAnsiDisplay.DetectTerminalSize;
{$IFDEF UNIX}
type
  TWinSize = record
    ws_row: Word;
    ws_col: Word;
    ws_xpixel: Word;
    ws_ypixel: Word;
  end;
  
const
  TIOCGWINSZ = $40087468; // ioctl command to get window size
  
var
  WS: TWinSize;
{$ENDIF}
begin
  // Default fallback values
  FTerminalWidth := 80;
  FTerminalHeight := 24;
  
  {$IFDEF UNIX}
  // Get actual terminal size using ioctl
  if fpioctl(STDOUT_FILENO, TIOCGWINSZ, @WS) = 0 then
  begin
    if (WS.ws_col > 0) and (WS.ws_row > 0) then
    begin
      FTerminalWidth := WS.ws_col;
      FTerminalHeight := WS.ws_row;
    end;
  end;
  {$ENDIF}
end;

procedure TAnsiDisplay.SetRawMode;
{$IFDEF UNIX}
var
  NewTermios: TermIOs;
{$ENDIF}
begin
  {$IFDEF UNIX}
  // Save original terminal settings
  TCGetAttr(STDIN_FILENO, FOriginalTermios);
  
  // Set up raw mode
  NewTermios := FOriginalTermios;
  NewTermios.c_lflag := NewTermios.c_lflag and (not (ICANON or ECHO));
  NewTermios.c_cc[VMIN] := 1;
  NewTermios.c_cc[VTIME] := 0;
  
  TCSetAttr(STDIN_FILENO, TCSANOW, NewTermios);
  {$ENDIF}
end;

procedure TAnsiDisplay.RestoreTerminal;
begin
  {$IFDEF UNIX}
  // Restore original terminal settings
  TCSetAttr(STDIN_FILENO, TCSANOW, FOriginalTermios);
  {$ENDIF}
end;

procedure TAnsiDisplay.WriteAnsiSequence(const Sequence: string);
begin
  // Write ANSI escape sequence directly
  system.write(Sequence);
  system.flush(output);
end;

procedure TAnsiDisplay.Clear;
begin
  // For differential rendering, just mark that we need a full redraw
  FNeedsFullRedraw := True;
end;

procedure TAnsiDisplay.Write(const S: string);
begin
  // Use differential rendering instead of direct write
  DifferentialWrite(S);
end;

procedure TAnsiDisplay.WriteLn(const S: string);
begin
  // Direct WriteLn - UTF-8 safe
  system.writeln(S);
end;

procedure TAnsiDisplay.MoveCursor(X, Y: integer);
var
  Sequence: string;
begin
  // ANSI escape sequence: ESC[row;colH
  Sequence := #27'[' + IntToStr(Y) + ';' + IntToStr(X) + 'H';
  WriteAnsiSequence(Sequence);
end;

function TAnsiDisplay.KeyPressed: boolean;
{$IFDEF UNIX}
var
  FDS: TFDSet;
  TimeVal: TTimeVal;
{$ENDIF}  
begin
  {$IFDEF UNIX}
  fpFD_ZERO(FDS);
  fpFD_SET(STDIN_FILENO, FDS);
  
  TimeVal.tv_sec := 0;
  TimeVal.tv_usec := 0;  // No timeout = immediate return
  
  Result := fpSelect(1, @FDS, nil, nil, @TimeVal) > 0;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function TAnsiDisplay.ReadKey: char;
var
  Ch: char;
begin
  {$IFDEF UNIX}
  // In raw mode, read single character without Enter
  read(Ch);
  Result := Ch;
  {$ELSE}
  // Fallback for non-Unix systems
  read(Ch);
  Result := Ch;
  {$ENDIF}
end;

function TAnsiDisplay.GetWidth: integer;
begin
  Result := FTerminalWidth;
end;

function TAnsiDisplay.GetHeight: integer;
begin
  Result := FTerminalHeight;
end;

procedure TAnsiDisplay.ClearToEndOfLine;
begin
  WriteAnsiSequence(#27'[K');
end;

procedure TAnsiDisplay.ClearFromCursor;
begin
  WriteAnsiSequence(#27'[J');
end;

procedure TAnsiDisplay.ForceFullRedraw;
begin
  FNeedsFullRedraw := True;
  SetLength(FLastRender, 0);
end;

function TAnsiDisplay.GetDisplayMode: TDisplayMode;
begin
  Result := FDisplayMode;
end;

procedure TAnsiDisplay.DifferentialWrite(const Content: string);
var
  NewLines: TStringArray;
  I, MaxLines: integer;
  Line: string;
  NeedsUpdate: Boolean;
begin
  // Split content into lines using the function from bobastyle
  NewLines := bobastyle.SplitLines(Content);
  
  // If this is the first render or we need a full redraw
  if FNeedsFullRedraw or (Length(FLastRender) = 0) then
  begin
    // Only clear screen in fullscreen mode
    if FDisplayMode = dmFullScreen then
    begin
      // Move cursor to home and clear screen
      WriteAnsiSequence(#27'[H'#27'[2J');
    end
    else if FDisplayMode = dmInline then
    begin
      // For inline mode, save cursor position and use it as our base
      WriteAnsiSequence(#27'[s'); // Save cursor position
      FInlineInitialized := True;
    end;
    
    // Write all lines
    for I := 0 to High(NewLines) do
    begin
      system.write(NewLines[I]);
      if I < High(NewLines) then
        system.write(LineEnding);
    end;
    
    // Update our last render buffer
    FLastRender := Copy(NewLines);
    FNeedsFullRedraw := False;
    system.flush(output);
    Exit;
  end;
  
  // Differential rendering: compare line by line
  MaxLines := Length(NewLines);
  if Length(FLastRender) > MaxLines then
    MaxLines := Length(FLastRender);
  
  for I := 0 to MaxLines - 1 do
  begin
    // Get current line (empty if beyond new content)
    if I < Length(NewLines) then
      Line := NewLines[I]
    else
      Line := '';
    
    // Check if this line needs updating
    NeedsUpdate := False;
    if I >= Length(FLastRender) then
      NeedsUpdate := True  // New line beyond previous content
    else if Line <> FLastRender[I] then
      NeedsUpdate := True; // Line content changed
    
    if NeedsUpdate then
    begin
      if FDisplayMode = dmFullScreen then
      begin
        // Move cursor to this line (1-based positioning)
        WriteAnsiSequence(#27'[' + IntToStr(I + 1) + ';1H');
      end
      else if FDisplayMode = dmInline then
      begin
        // For inline mode, position relative to our saved starting position
        WriteAnsiSequence(#27'[u'); // Restore cursor position
        if I > 0 then
        begin
          // Move down I lines from the saved position
          WriteAnsiSequence(#27'[' + IntToStr(I) + 'B'); // Move down I lines
        end;
        WriteAnsiSequence(#27'[1G'); // Move to column 1
      end;
      
      // Write the new line content
      system.write(Line);
      
      // Always clear to end of line when content changes to handle ANSI styled content properly
      // The original logic only cleared when new line was shorter, but ANSI codes make 
      // byte length comparison unreliable for determining visual width changes
      if (I < Length(FLastRender)) then
        ClearToEndOfLine;
    end;
  end;
  
  // If new content has fewer lines, clear the remaining old lines
  if Length(FLastRender) > Length(NewLines) then
  begin
    for I := Length(NewLines) to Length(FLastRender) - 1 do
    begin
      if FDisplayMode = dmFullScreen then
      begin
        WriteAnsiSequence(#27'[' + IntToStr(I + 1) + ';1H');
      end
      else if FDisplayMode = dmInline then
      begin
        // For inline mode, position relative to our saved starting position
        WriteAnsiSequence(#27'[u'); // Restore cursor position
        if I > 0 then
        begin
          // Move down I lines from the saved position
          WriteAnsiSequence(#27'[' + IntToStr(I) + 'B'); // Move down I lines
        end;
        WriteAnsiSequence(#27'[1G'); // Move to column 1
      end;
      ClearToEndOfLine;
    end;
  end;
  
  // Update our last render buffer
  FLastRender := Copy(NewLines);
  system.flush(output);
end;

constructor TInputThread.Create(AMessageQueue: TMessageQueue; ADisplay: TDisplay);
begin
  inherited Create(True); // Create suspended
  FMessageQueue := AMessageQueue;
  FDisplay := ADisplay;
  FreeOnTerminate := False;
end;

constructor TTimerThread.Create(AMessageQueue: TMessageQueue);
begin
  inherited Create(True); // Create suspended
  FMessageQueue := AMessageQueue;
  FreeOnTerminate := False;
  FComponentTickCS := TCriticalSection.Create;
  SetLength(FComponentTicks, 0);
end;

destructor TTimerThread.Destroy;
begin
  FComponentTickCS.Free;
  inherited Destroy;
end;


procedure TTimerThread.ScheduleComponentTick(const ComponentId: string; Interval: integer);
var
  I: integer;
  Found: Boolean;
  NewTick: TComponentTick;
begin
  FComponentTickCS.Enter;
  try
    Found := False;
    // Update existing component tick or add new one
    for I := 0 to High(FComponentTicks) do
    begin
      if FComponentTicks[I].ComponentId = ComponentId then
      begin
        FComponentTicks[I].Interval := Interval;
        FComponentTicks[I].NextTick := Now + (Interval / (1000.0 * 24 * 60 * 60)); // Convert ms to days
        Found := True;
        Break;
      end;
    end;
    
    if not Found then
    begin
      NewTick.ComponentId := ComponentId;
      NewTick.Interval := Interval;
      NewTick.NextTick := Now + (Interval / (1000.0 * 24 * 60 * 60));
      SetLength(FComponentTicks, Length(FComponentTicks) + 1);
      FComponentTicks[High(FComponentTicks)] := NewTick;
    end;
  finally
    FComponentTickCS.Leave;
  end;
end;

procedure TTimerThread.Execute;
var
  CurrentTime: TDateTime;
  I: integer;
begin
  while not Terminated do
  begin
    try
      CurrentTime := Now;
      
      // Check component ticks
      FComponentTickCS.Enter;
      try
        for I := 0 to High(FComponentTicks) do
        begin
          if CurrentTime >= FComponentTicks[I].NextTick then
          begin
            // Send component tick message
            FMessageQueue.Push(TComponentTickMsg.Create(FComponentTicks[I].ComponentId, FComponentTicks[I].Interval));
            // Schedule next tick
            FComponentTicks[I].NextTick := CurrentTime + (FComponentTicks[I].Interval / (1000.0 * 24 * 60 * 60));
          end;
        end;
      finally
        FComponentTickCS.Leave;
      end;
      
      // Sleep for a reasonable amount of time between tick checks
      Sleep(10);
    except
      // Ignore exceptions during thread termination
      Break;
    end;
  end;
end;

procedure TInputThread.Execute;
var
  Ch: char;
begin
  while not Terminated do
  begin
    try
      if Assigned(FMessageQueue) and Assigned(FDisplay) and FDisplay.KeyPressed then
      begin
        Ch := FDisplay.ReadKey;
        if Assigned(FMessageQueue) then // Check again in case it was freed
        begin
          // Check for escape sequences (arrow keys)
          if Ch = #27 then  // ESC character
          begin
            // Read the next character immediately - like the original working version
            Ch := FDisplay.ReadKey;  // Should be '['
            if Ch = '[' then
            begin
              // Read the final character immediately
              Ch := FDisplay.ReadKey;  // Should be A, B, C, or D
              case Ch of
                'A': FMessageQueue.Push(TKeyMsg.Create(#27'[A'));  // Up arrow
                'B': FMessageQueue.Push(TKeyMsg.Create(#27'[B'));  // Down arrow  
                'C': FMessageQueue.Push(TKeyMsg.Create(#27'[C'));  // Right arrow
                'D': FMessageQueue.Push(TKeyMsg.Create(#27'[D'));  // Left arrow
                else
                  // Not a valid arrow key, treat as ESC
                  FMessageQueue.Push(TKeyMsg.Create(#27));
              end;
            end
            else
            begin
              // Second character wasn't '[', treat as ESC and push back the character
              FMessageQueue.Push(TKeyMsg.Create(#27));
              FMessageQueue.Push(TKeyMsg.Create(Ch));
            end;
          end
          else
          begin
            // Regular character
            FMessageQueue.Push(TKeyMsg.Create(Ch));
          end;
        end;
      end
      else
      begin
        // Use a more responsive sleep when no input is available
        Sleep(1);
      end;
    except
      // Ignore exceptions during thread termination
      Break;
    end;
  end;
end;

constructor TBobaUIProgram.Create(AModel: TModel; ADisplay: TDisplay);
begin
  inherited Create;
  FModel := AModel;
  FDisplay := ADisplay;
  FRunning := true;
  FMessageQueue := TMessageQueue.Create;
  FInputThread := TInputThread.Create(FMessageQueue, FDisplay);
  FTimerThread := TTimerThread.Create(FMessageQueue);
  
  {$IFDEF UNIX}
  GlobalProgram := Pointer(Self);
  InitializeSignalHandlers;
  {$ENDIF}
end;

constructor TBobaUIProgram.Create(AModel: TModel; ADisplayMode: TDisplayMode);
begin
  inherited Create;
  FModel := AModel;
  FDisplay := TAnsiDisplay.Create(ADisplayMode);
  FRunning := true;
  FMessageQueue := TMessageQueue.Create;
  FInputThread := TInputThread.Create(FMessageQueue, FDisplay);
  FTimerThread := TTimerThread.Create(FMessageQueue);
  
  {$IFDEF UNIX}
  GlobalProgram := Pointer(Self);
  InitializeSignalHandlers;
  {$ENDIF}
end;

destructor TBobaUIProgram.Destroy;
begin
  try
    // Set running to false first to stop main loop
    FRunning := false;
    
    {$IFDEF UNIX}
    // Clear global reference immediately to prevent signal handler access
    GlobalProgram := nil;
    try
      RestoreSignalHandlers;
    except
      // Ignore signal handler cleanup errors
    end;
    {$ENDIF}
    
    // Terminate and wait for input thread
    if Assigned(FInputThread) then
    begin
      try
        FInputThread.Terminate;
        // Give thread time to finish gracefully
        Sleep(100);
        FInputThread.WaitFor;
        FInputThread.Free;
      except
        // Ignore thread cleanup errors
      end;
      FInputThread := nil;
    end;
    
    // Terminate and wait for timer thread
    if Assigned(FTimerThread) then
    begin
      try
        FTimerThread.Terminate;
        // Give thread time to finish gracefully
        Sleep(100);
        FTimerThread.WaitFor;
        FTimerThread.Free;
      except
        // Ignore thread cleanup errors
      end;
      FTimerThread := nil;
    end;
    
    // Shutdown and free message queue after thread is terminated
    if Assigned(FMessageQueue) then
    begin
      try
        FMessageQueue.Shutdown;
        FMessageQueue.Free;
      except
        // Ignore message queue cleanup errors
      end;
      FMessageQueue := nil;
    end;
      
    if Assigned(FModel) then
    begin
      try
        FModel.Free;
      except
        // Ignore model cleanup errors
      end;
      FModel := nil;
    end;
    if Assigned(FDisplay) then
    begin
      try
        FDisplay.Free;
      except
        // Ignore display cleanup errors
      end;
      FDisplay := nil;
    end;
  except
    // Ignore all cleanup errors to prevent access violations
  end;
  inherited Destroy;
end;

procedure TBobaUIProgram.Quit;
begin
  FRunning := false;
end;


procedure TBobaUIProgram.ScheduleComponentTick(const ComponentId: string; Interval: integer);
begin
  if Assigned(FTimerThread) then
    FTimerThread.ScheduleComponentTick(ComponentId, Interval);
end;

{$IFDEF UNIX}
procedure TBobaUIProgram.InitializeSignalHandlers;
var
  NewAction: PSigActionRec;
begin
  New(NewAction);
  New(FOriginalSigWinchHandler);
  
  NewAction^.sa_Handler := SigActionHandler(@HandleSIGWINCH);
  fillchar(NewAction^.Sa_Mask, sizeof(NewAction^.sa_mask), #0);
  NewAction^.Sa_Flags := 0;
  {$ifdef Linux}
  NewAction^.Sa_Restorer := nil;
  {$endif}
  
  if fpSigAction(SIGWINCH, NewAction, FOriginalSigWinchHandler) < 0 then
  begin
    WriteLn('Warning: Could not set SIGWINCH handler');
  end;
  
  Dispose(NewAction);
end;

procedure TBobaUIProgram.RestoreSignalHandlers;
begin
  if Assigned(FOriginalSigWinchHandler) then
  begin
    fpSigAction(SIGWINCH, FOriginalSigWinchHandler, nil);
    Dispose(FOriginalSigWinchHandler);
  end;
end;
{$ENDIF}

procedure TBobaUIProgram.HandleWindowResize(Width, Height: integer);
begin
  if Assigned(FMessageQueue) then
    FMessageQueue.Push(TWindowSizeMsg.Create(Width, Height));
end;

// Command implementations
function DoQuitCmd: TMsg; cdecl;
begin
  Result := TQuitMsg.Create;
end;

function DoShowCursorCmd: TMsg; cdecl;
begin
  Result := TShowCursorMsg.Create;
end;

function DoHideCursorCmd: TMsg; cdecl;
begin
  Result := THideCursorMsg.Create;
end;

function DoTickCmd(Interval: integer): TMsg; cdecl;
begin
  Result := TTickMsg.Create(Now, Interval);
end;

function DoComponentTickCmd: TMsg; cdecl;
begin
  // Schedule the component tick using global state
  if Assigned(GlobalProgram) then
    TBobaUIProgram(GlobalProgram).ScheduleComponentTick(PendingComponentId, PendingComponentInterval);
  Result := nil; // Don't return a message, just schedule the tick
end;

function QuitCmd: TCmd;
begin
  Result := @DoQuitCmd;
end;

function ShowCursorCmd: TCmd;
begin
  Result := @DoShowCursorCmd;
end;

function HideCursorCmd: TCmd;
begin
  Result := @DoHideCursorCmd;
end;

function TickCmd(Interval: integer): TCmd;
begin
  // We need a different approach for parameterized commands
  // For now, we'll create the message directly
  Result := nil; // This will need special handling in Update
end;

function ComponentTickCmd(const ComponentId: string; Interval: integer): TCmd;
begin
  // Set global state for the component tick
  PendingComponentId := ComponentId;
  PendingComponentInterval := Interval;
  Result := @DoComponentTickCmd;
end;

// Global state for overlay commands
var
  PendingOverlayContent: TModel = nil;
  PendingOverlayPosition: TRect;
  PendingOverlayZIndex: Integer = 100;

function DoAddOverlayCmd: TMsg; cdecl;
begin
  Result := TAddOverlayMsg.Create(PendingOverlayContent, PendingOverlayPosition, PendingOverlayZIndex);
  PendingOverlayContent := nil; // Reset after use
end;

function DoRemoveTopOverlayCmd: TMsg; cdecl;
begin
  Result := TRemoveTopOverlayMsg.Create;
end;

function DoClearOverlaysCmd: TMsg; cdecl;
begin
  Result := TClearOverlaysMsg.Create;
end;

function AddOverlayCmd(Content: TModel; Position: TRect; ZIndex: Integer): TCmd;
begin
  PendingOverlayContent := Content;
  PendingOverlayPosition := Position;
  PendingOverlayZIndex := ZIndex;
  Result := @DoAddOverlayCmd;
end;

function RemoveTopOverlayCmd: TCmd;
begin
  Result := @DoRemoveTopOverlayCmd;
end;

function ClearOverlaysCmd: TCmd;
begin
  Result := @DoClearOverlaysCmd;
end;

function CenterRect(Width, Height: Integer): TRect;
begin
  // Use default terminal size for centering
  // In a real application, you'd pass the actual screen dimensions
  Result.X := (80 - Width) div 2 + 1;  // +1 for 1-based positioning
  Result.Y := (24 - Height) div 2 + 1;
  Result.Width := Width;
  Result.Height := Height;
  
  // Ensure we don't go off-screen
  if Result.X < 1 then Result.X := 1;
  if Result.Y < 1 then Result.Y := 1;
end;

function CenterRectInBounds(Width, Height, ScreenWidth, ScreenHeight: Integer): TRect;
begin
  Result.X := (ScreenWidth - Width) div 2 + 1;  // +1 for 1-based positioning
  Result.Y := (ScreenHeight - Height) div 2 + 1;
  Result.Width := Width;
  Result.Height := Height;
  
  // Ensure we don't go off-screen
  if Result.X < 1 then Result.X := 1;
  if Result.Y < 1 then Result.Y := 1;
end;

function PositionRectBelowMenu(MenuItemX, DropdownWidth, DropdownHeight, ScreenWidth, ScreenHeight: Integer): TRect;
begin
  Result.X := MenuItemX;
  Result.Y := 2; // Position below the menu bar (menu bar is at Y=1)
  Result.Width := DropdownWidth;
  Result.Height := DropdownHeight;
  
  // Ensure dropdown doesn't go off-screen horizontally
  if Result.X + DropdownWidth > ScreenWidth then
    Result.X := ScreenWidth - DropdownWidth + 1;
  if Result.X < 1 then Result.X := 1;
  
  // Ensure dropdown doesn't go off-screen vertically
  if Result.Y + DropdownHeight > ScreenHeight then
    Result.Y := ScreenHeight - DropdownHeight + 1;
  if Result.Y < 1 then Result.Y := 1;
end;

function CreateRect(X, Y, Width, Height: Integer): TRect;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;

{$IFDEF UNIX}
procedure GetCurrentTerminalSize(out Width, Height: integer);
type
  TWinSize = record
    ws_row: Word;
    ws_col: Word;
    ws_xpixel: Word;
    ws_ypixel: Word;
  end;
  
const
  TIOCGWINSZ = $40087468; // ioctl command to get window size
  
var
  WS: TWinSize;
begin
  // Default fallback values
  Width := 80;
  Height := 24;
  
  // Get actual terminal size using ioctl
  if fpioctl(STDOUT_FILENO, TIOCGWINSZ, @WS) = 0 then
  begin
    if (WS.ws_col > 0) and (WS.ws_row > 0) then
    begin
      Width := WS.ws_col;
      Height := WS.ws_row;
    end;
  end;
end;

procedure HandleSIGWINCH(sig: cint); cdecl;
var
  Width, Height: integer;
begin
  // Signal handler - must be minimal and signal-safe
  if Assigned(GlobalProgram) then
  begin
    GetCurrentTerminalSize(Width, Height);
    TBobaUIProgram(GlobalProgram).HandleWindowResize(Width, Height);
  end;
end;
{$ENDIF}

procedure TBobaUIProgram.Run;
var
  UpdateResult: TUpdateResult;
  Msg: TMsg;
  CmdMsg: TMsg;
  OldModel: TModel;
  InitialSize: TWindowSizeMsg;
  NewContent: string;
begin
  // Start the input thread
  FInputThread.Start;
  
  // Start the timer thread
  FTimerThread.Start;
  
  // Send initial window size message
  InitialSize := TWindowSizeMsg.Create(FDisplay.GetWidth, FDisplay.GetHeight);
  FMessageQueue.Push(InitialSize);
  
  // Initial draw before the loop starts.
  if FModel.HasOverlays then
    NewContent := FModel.RenderWithOverlays
  else
    NewContent := FModel.View;
  FDisplay.Write(NewContent);
  // Hide cursor by moving it to bottom of screen (fullscreen mode only)
  if FDisplay is TAnsiDisplay then
  begin
    if TAnsiDisplay(FDisplay).GetDisplayMode = dmFullScreen then
      FDisplay.MoveCursor(1, FDisplay.GetHeight);
  end;

  repeat
    try
      // Check for messages in the queue
      Msg := nil;
      if Assigned(FMessageQueue) then
        Msg := FMessageQueue.Pop;
      
      if Assigned(Msg) then
      begin
        OldModel := FModel;
        
        // First try to handle overlay messages
        UpdateResult := FModel.UpdateOverlays(Msg);
        
        // If overlays didn't handle the message, pass to main Update
        if UpdateResult.Model = FModel then
          UpdateResult := FModel.Update(Msg);
        Msg.Free;
        Msg := nil;

        // If Update returned a new model instance, manage the memory.
        if UpdateResult.Model <> OldModel then
        begin
          if Assigned(OldModel) then
            OldModel.Free;
          FModel := UpdateResult.Model;
        end;

        // Execute any command returned by Update.
        if Assigned(UpdateResult.Cmd) then
        begin
          CmdMsg := UpdateResult.Cmd();
          if Assigned(CmdMsg) then
          begin
            if CmdMsg is TQuitMsg then
            begin
              Quit;
            end
            else if CmdMsg is TShowCursorMsg then
            begin
              if FDisplay is TAnsiDisplay then
                TAnsiDisplay(FDisplay).WriteAnsiSequence(#27'[?25h');
            end
            else if CmdMsg is THideCursorMsg then
            begin
              if FDisplay is TAnsiDisplay then
                TAnsiDisplay(FDisplay).WriteAnsiSequence(#27'[?25l');
            end;
            CmdMsg.Free;
            CmdMsg := nil;
          end;
        end;

        // After an update, if we're still running, redraw the screen.
        if FRunning and Assigned(FDisplay) and Assigned(FModel) then
        begin
          // Differential renderer handles the optimization automatically
          if FModel.HasOverlays then
            NewContent := FModel.RenderWithOverlays
          else
            NewContent := FModel.View;
          FDisplay.Write(NewContent);
          // Hide cursor by moving it to bottom of screen (fullscreen mode only)
          if FDisplay is TAnsiDisplay then
          begin
            if TAnsiDisplay(FDisplay).GetDisplayMode = dmFullScreen then
              FDisplay.MoveCursor(1, FDisplay.GetHeight);
          end;
        end;
      end
      else
      begin
        // No messages available, small sleep to prevent busy waiting
        Sleep(10);
      end;
    except
      // Handle any exceptions gracefully during shutdown
      if not FRunning then
        Break;
      Sleep(10);
    end;
  until not FRunning;
end;

end. 
