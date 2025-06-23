unit bobaui;

{$mode objfpc}
{$codepage UTF8}
{$H+}

{* Commented out debug logging
{$DEFINE BOBAUI_DEBUG} // Enable debug logging
*}

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
  
  // ANSI Escape Sequences
  // Basic cursor operations
  ANSI_CURSOR_QUERY = #27'[6n';          // Query cursor position
  ANSI_CURSOR_SAVE = #27'[s';            // Save cursor position
  ANSI_CURSOR_RESTORE = #27'[u';         // Restore cursor position
  ANSI_CURSOR_COLUMN_1 = #27'[1G';       // Move cursor to column 1
  ANSI_CURSOR_SHOW = #27'[?25h';         // Show cursor
  ANSI_CURSOR_HIDE = #27'[?25l';         // Hide cursor
  
  // Screen operations
  ANSI_CLEAR_TO_EOL = #27'[K';           // Clear from cursor to end of line
  ANSI_CLEAR_FROM_CURSOR = #27'[J';      // Clear from cursor to end of screen
  ANSI_CLEAR_HOME_AND_SCREEN = #27'[H'#27'[2J';  // Move to home and clear screen
  
  // Screen buffer management
  ANSI_ALT_SCREEN_ON = #27'[?1049h';     // Switch to alternate screen buffer
  ANSI_ALT_SCREEN_OFF = #27'[?1049l';    // Switch back to main screen buffer
  
  // Arrow key sequences
  ANSI_ARROW_UP = #27'[A';               // Up arrow key
  ANSI_ARROW_DOWN = #27'[B';             // Down arrow key
  ANSI_ARROW_RIGHT = #27'[C';            // Right arrow key
  ANSI_ARROW_LEFT = #27'[D';             // Left arrow key
  
  // Color/style resets
  ANSI_RESET_ALL = #27'[0m';             // Reset all formatting
  ANSI_RESET_SHORT = #27'[m';            // Reset all formatting (short form)
  ANSI_RESET_FOREGROUND = #27'[39m';     // Reset foreground color
  ANSI_RESET_BACKGROUND = #27'[49m';     // Reset background color
  ANSI_RESET_FG_BG = #27'[39;49m';       // Reset both foreground and background
  ANSI_RESET_BG_FG = #27'[49;39m';       // Reset both background and foreground
  
  // Escape sequence prefix for building dynamic sequences
  ANSI_ESC_PREFIX = #27'[';              // Standard ANSI escape prefix

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

  // Forward declaration for command type
  TCommand = class;
  TCmd = TCommand;

  // Batch message for executing multiple commands sequentially
  TBatchMsg = class(TMsg)
  private
    FCommands: array of TCmd;
    function GetCommand(Index: Integer): TCmd;
    function GetCommandCount: Integer;
  public
    constructor Create(const ACommands: array of TCmd);
    destructor Destroy; override;
    property Commands[Index: Integer]: TCmd read GetCommand;
    property CommandCount: Integer read GetCommandCount;
  end;

  // Command pattern implementation
  TCommand = class
  public
    function Execute: TMsg; virtual; abstract;
  end;

  // Simple commands
  TQuitCommand = class(TCommand)
  public
    function Execute: TMsg; override;
  end;

  TShowCursorCommand = class(TCommand)
  public
    function Execute: TMsg; override;
  end;

  THideCursorCommand = class(TCommand)
  public
    function Execute: TMsg; override;
  end;

  // Schedule tick message - internal message for tick scheduling
  TScheduleTickMsg = class(TMsg)
  private
    FComponentId: string;
    FInterval: Integer;
  public
    constructor Create(const AComponentId: string; AInterval: Integer);
    property ComponentId: string read FComponentId;
    property Interval: Integer read FInterval;
  end;

  // Tick command
  TTickCommand = class(TCommand)
  private
    FComponentId: string;
    FInterval: Integer;
  public
    constructor Create(const AComponentId: string; AInterval: Integer);
    function Execute: TMsg; override;
  end;

  // Batch command for executing multiple commands sequentially
  TBatchCommand = class(TCommand)
  private
    FCommands: array of TCmd;
  public
    constructor Create(const ACommands: array of TCmd);
    destructor Destroy; override;
    function Execute: TMsg; override;
  end;

  // Message queue for thread-safe communication
  TMessageQueue = class
  private
    FInputMsgObjects: array of TMsg;  // High priority queue for input messages
    FInputCount: integer;
    FAnimationMsgObjects: array of TMsg;  // Lower priority queue for animation messages
    FAnimationCount: integer;
    FCS: TCriticalSection;
    FDestroying: Boolean;
    function ShouldDropMessage: Boolean;
    function IsHighPriorityMessage(const Msg: TMsg): Boolean;
    procedure AddToInputQueue(const Msg: TMsg);
    procedure AddToAnimationQueue(const Msg: TMsg);
    procedure TrimAnimationQueue;
    procedure ResizeInputQueueIfNeeded;
    procedure ResizeAnimationQueueIfNeeded;
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
  TAnsiDisplay = class;
  TDisplay = class;
  TCursorPosition = class;

  // Cursor position manager for centralized position tracking
  TCursorPosition = class
  private
    FX, FY: Integer;
    FInlineBaseX, FInlineBaseY: Integer;
    FInlineInitialized: Boolean;
    FDisplayMode: TDisplayMode;
    FDisplay: TDisplay;
  public
    constructor Create(ADisplayMode: TDisplayMode; ADisplay: TDisplay);
    procedure SetPosition(X, Y: Integer);
    procedure GetPosition(out X, Y: Integer);
    function GetX: Integer;
    function GetY: Integer;
    procedure SetInlineBase(X, Y: Integer);
    procedure GetInlineBase(out X, Y: Integer);
    function IsInlineInitialized: Boolean;
    procedure MarkInlineInitialized;
    function QueryRealPosition: Boolean;
    property DisplayMode: TDisplayMode read FDisplayMode;
  end;

  // Abstract base class for rendering strategies
  TRenderingStrategy = class
  protected
    FDisplay: TAnsiDisplay;
    FCursorPosition: TCursorPosition;
  public
    constructor Create(ADisplay: TAnsiDisplay; ACursorPosition: TCursorPosition);
    procedure InitializeFirstRender; virtual; abstract;
    procedure PositionForLine(LineIndex: Integer); virtual; abstract;
    procedure FinalizeLine; virtual; abstract;
    procedure Cleanup; virtual; abstract;
  end;

  // Fullscreen rendering strategy
  TFullscreenRenderingStrategy = class(TRenderingStrategy)
  public
    procedure InitializeFirstRender; override;
    procedure PositionForLine(LineIndex: Integer); override;
    procedure FinalizeLine; override;
    procedure Cleanup; override;
  end;

  // Inline rendering strategy  
  TInlineRenderingStrategy = class(TRenderingStrategy)
  public
    constructor Create(ADisplay: TAnsiDisplay; ACursorPosition: TCursorPosition);
    procedure InitializeFirstRender; override;
    procedure PositionForLine(LineIndex: Integer); override;
    procedure FinalizeLine; override;
    procedure Cleanup; override;
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
    procedure WriteAnsiSequence(const Sequence: string); virtual; abstract;
    procedure MoveCursor(X, Y: integer); virtual; abstract;
    
    // Input operations
    function ReadKey: char; virtual; abstract;
    function KeyPressed: boolean; virtual; abstract;
    
    // Screen dimensions
    function GetWidth: integer; virtual; abstract;
    function GetHeight: integer; virtual; abstract;
  end;

  // Component classes for TAnsiDisplay separation of concerns
  
  // Terminal setup/teardown and size detection
  TTerminalManager = class
  private
    FWidth, FHeight: Integer;
    {$IFDEF UNIX}
    FOriginalTermios: TermIOs;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetRawMode;
    procedure RestoreTerminal;
    procedure DetectTerminalSize;
    function GetWidth: Integer;
    function GetHeight: Integer;
  end;
  
  // ANSI sequence generation and writing
  TAnsiRenderer = class
  public
    procedure WriteAnsiSequence(const Sequence: string);
    procedure MoveCursor(X, Y: Integer);
    procedure ClearToEndOfLine;
    procedure ClearFromCursor;
  end;
  
  // Rendering optimization logic
  TDifferentialRenderer = class
  private
    FLastRender: TStringArray;
    FNeedsFullRedraw: Boolean;
    FRenderingStrategy: TRenderingStrategy;
    FAnsiRenderer: TAnsiRenderer;
  public
    constructor Create(AAnsiRenderer: TAnsiRenderer; ARenderingStrategy: TRenderingStrategy);
    destructor Destroy; override;
    procedure DifferentialWrite(const Content: string);
    procedure HandleFirstRender(const NewLines: TStringArray);
    procedure UpdateChangedLines(const NewLines: TStringArray);
    procedure ClearRemainingLines(const NewLines: TStringArray);
    function ShouldUpdateLine(LineIndex: Integer; const NewLine: string): Boolean;
    procedure ForceFullRedraw;
  end;

  // ANSI terminal implementation (UTF-8 safe, no CRT dependency)
  TAnsiDisplay = class(TDisplay)
  private
    FDisplayMode: TDisplayMode;
    FCursorPosition: TCursorPosition;
    // Component instances for separation of concerns
    FTerminalManager: TTerminalManager;
    FAnsiRenderer: TAnsiRenderer;
    FDifferentialRenderer: TDifferentialRenderer;
    FRenderingStrategy: TRenderingStrategy;
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
    procedure WriteAnsiSequence(const Sequence: string); override;
    procedure ClearToEndOfLine;
    procedure ClearFromCursor;
    procedure ForceFullRedraw;
    function GetDisplayMode: TDisplayMode;
    procedure SetDisplayMode(AMode: TDisplayMode);
  end;

  // Base model type that all applications will extend
  TModel = class
  private
    FOverlays: array of TOverlay;
    procedure SortOverlaysByZIndex;
    function CompositeOverlay(var BaseLines: TStringArray; const Overlay: TOverlay): Boolean;
    function ValidateOverlay(const Overlay: TOverlay): Boolean;
    procedure PrepareOverlayData(const Overlay: TOverlay; out OverlayLines: TStringArray; out OverlayX, OverlayY: Integer);
    function CompositeOverlayLine(var BaseLines: TStringArray; const OverlayLine: string; TargetY, OverlayX: Integer): Boolean;
    function PrepareBaseLineForOverlay(const BaseLine: string; OverlayX: Integer): string;
    function SplitBaseLineForOverlay(const BaseLine: string; OverlayX: Integer; OverlayWidth: Integer; out BeforePart, AfterPart: string): Boolean;
    function RestoreColorsAfterOverlay(const AfterPart, ActiveBgColor, ActiveFgColor: string): string;
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

  // Overlay commands (declared after TModel)
  TAddOverlayCommand = class(TCommand)
  private
    FContent: TModel;
    FPosition: TRect;
    FZIndex: Integer;
  public
    constructor Create(AContent: TModel; APosition: TRect; AZIndex: Integer);
    function Execute: TMsg; override;
  end;

  TRemoveTopOverlayCommand = class(TCommand)
  public
    function Execute: TMsg; override;
  end;

  TClearOverlaysCommand = class(TCommand)
  public
    function Execute: TMsg; override;
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
  // Input parsing abstraction - separates parsing logic from threading concerns
  TInputParser = class
  private
    FDisplay: TDisplay;
    FPendingChar: char;
    FHasPendingChar: Boolean;
    function ReadNextChar: char;
    function ParseArrowSequence(FirstChar: char): char;
    function CreateArrowKeyMessage(ArrowChar: char): TKeyMsg;
  public
    constructor Create(ADisplay: TDisplay);
    function ParseInput(InitialChar: char): TKeyMsg;
    function HasPendingInput: Boolean;
    function GetPendingInput: TKeyMsg;
  end;

  TInputThread = class(TThread)
  private
    FMessageQueue: TMessageQueue;
    FDisplay: TDisplay;
    FInputParser: TInputParser;
  protected
    procedure Execute; override;
  public
    constructor Create(AMessageQueue: TMessageQueue; ADisplay: TDisplay);
    destructor Destroy; override;
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
    procedure SetDisplayMode(AMode: TDisplayMode);
    procedure Send(const Msg: TMsg);
  end;

// Common commands
function QuitCmd: TCmd;
function ShowCursorCmd: TCmd;
function HideCursorCmd: TCmd;

// Command batching
function BatchCmd(const Commands: array of TCmd): TCmd;

// Animation commands
function TickCmd(const ComponentId: string; Interval: integer): TCmd; // Create tick command

// Overlay commands
function AddOverlayCmd(Content: TModel; Position: TRect; ZIndex: Integer = 100): TCmd;
function RemoveTopOverlayCmd: TCmd;
function ClearOverlaysCmd: TCmd;

// ANSI sequence helper functions
function AnsiMoveCursor(X, Y: Integer): string;
function AnsiMoveDown(Lines: Integer): string;

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

// Command functions are now implemented as command objects

{$IFDEF UNIX}
// Signal handler for SIGWINCH (terminal resize)
procedure HandleSIGWINCH(sig: cint); cdecl;

// Function to get current terminal size
procedure GetCurrentTerminalSize(out Width, Height: integer);

// Program registration functions for signal handling
procedure RegisterProgram(AProgram: TBobaUIProgram);
procedure UnregisterProgram(AProgram: TBobaUIProgram);
{$ENDIF}

var
  // Global list of active program instances for signal dispatching
  ActivePrograms: array of TBobaUIProgram;
  ActiveProgramsCS: TCriticalSection;

implementation

{$IFDEF BOBAUI_DEBUG}
var
  BDebugFile: TextFile;
  BDebugInitialized: Boolean = False;

procedure InitBDebugLog;
begin
  if not BDebugInitialized then
  begin
    AssignFile(BDebugFile, 'bobaui_debug.log');
    Rewrite(BDebugFile);
    BDebugInitialized := True;
    WriteLn(BDebugFile, 'BobaUI Debug Log Started: ', DateTimeToStr(Now));
    WriteLn(BDebugFile, '=======================');
    Flush(BDebugFile);
  end;
end;

procedure BDebugLog(const Msg: string);
begin
  if not BDebugInitialized then
    InitBDebugLog;
  WriteLn(BDebugFile, '[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', Msg);
  Flush(BDebugFile);
end;

procedure CloseBDebugLog;
begin
  if BDebugInitialized then
  begin
    WriteLn(BDebugFile, '=======================');
    WriteLn(BDebugFile, 'BobaUI Debug Log Ended');
    CloseFile(BDebugFile);
    BDebugInitialized := False;
  end;
end;
{$ENDIF}

constructor TCursorPosition.Create(ADisplayMode: TDisplayMode; ADisplay: TDisplay);
begin
  inherited Create;
  FDisplayMode := ADisplayMode;
  FDisplay := ADisplay;
  FX := 1;
  FY := 1;
  FInlineBaseX := 1;
  FInlineBaseY := 1;
  FInlineInitialized := False;
end;

procedure TCursorPosition.SetPosition(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;

procedure TCursorPosition.GetPosition(out X, Y: Integer);
begin
  X := FX;
  Y := FY;
end;

function TCursorPosition.GetX: Integer;
begin
  Result := FX;
end;

function TCursorPosition.GetY: Integer;
begin
  Result := FY;
end;

procedure TCursorPosition.SetInlineBase(X, Y: Integer);
begin
  FInlineBaseX := X;
  FInlineBaseY := Y;
end;

procedure TCursorPosition.GetInlineBase(out X, Y: Integer);
begin
  X := FInlineBaseX;
  Y := FInlineBaseY;
end;

function TCursorPosition.IsInlineInitialized: Boolean;
begin
  Result := FInlineInitialized;
end;

procedure TCursorPosition.MarkInlineInitialized;
begin
  FInlineInitialized := True;
end;

function TCursorPosition.QueryRealPosition: Boolean;
{$IFDEF UNIX}
var
  C: Char;
  Response: string;
  SemicolonPos: Integer;
  RealX, RealY: Integer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF UNIX}
  try
    // Send cursor position query
    FDisplay.WriteAnsiSequence(ANSI_CURSOR_QUERY);
    
    // Read response: ESC[row;colR
    Response := '';
    
    // Read the escape sequence
    C := FDisplay.ReadKey;
    if C = #27 then  // ESC
    begin
      C := FDisplay.ReadKey;
      if C = '[' then  // [
      begin
        // Read until 'R'
        repeat
          C := FDisplay.ReadKey;
          if C <> 'R' then
            Response := Response + C;
        until C = 'R';
        
        // Parse "row;col" format
        SemicolonPos := Pos(';', Response);
        if SemicolonPos > 0 then
        begin
          RealY := StrToIntDef(Copy(Response, 1, SemicolonPos - 1), 1);
          RealX := StrToIntDef(Copy(Response, SemicolonPos + 1, Length(Response)), 1);
          SetPosition(RealX, RealY);
          Result := True;
        end;
      end;
    end;
  except
    // If anything fails, keep the current position
    Result := False;
  end;
  {$ENDIF}
end;

// Rendering Strategy implementations

constructor TRenderingStrategy.Create(ADisplay: TAnsiDisplay; ACursorPosition: TCursorPosition);
begin
  inherited Create;
  FDisplay := ADisplay;
  FCursorPosition := ACursorPosition;
end;

// Fullscreen rendering strategy
procedure TFullscreenRenderingStrategy.InitializeFirstRender;
begin
  // Move cursor to home and clear screen
  FDisplay.WriteAnsiSequence(ANSI_CLEAR_HOME_AND_SCREEN);
end;

procedure TFullscreenRenderingStrategy.PositionForLine(LineIndex: Integer);
begin
  // Move cursor to this line (1-based positioning)
  FDisplay.WriteAnsiSequence(AnsiMoveCursor(1, LineIndex + 1));
  // Update tracked cursor position
  FCursorPosition.SetPosition(1, LineIndex + 1);
end;

procedure TFullscreenRenderingStrategy.FinalizeLine;
begin
  // Nothing special needed for fullscreen
end;

procedure TFullscreenRenderingStrategy.Cleanup;
begin
  // Nothing special needed for fullscreen
end;

// Inline rendering strategy
constructor TInlineRenderingStrategy.Create(ADisplay: TAnsiDisplay; ACursorPosition: TCursorPosition);
begin
  inherited Create(ADisplay, ACursorPosition);
end;

procedure TInlineRenderingStrategy.InitializeFirstRender;
begin
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('TInlineRenderingStrategy.InitializeFirstRender - Entry');
  BDebugLog('  Terminal height: ' + IntToStr(FDisplay.GetHeight));
  {$ENDIF}
  
  // Query real cursor position to get accurate starting point
  if FCursorPosition.QueryRealPosition then
  begin
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Real cursor position: (' + IntToStr(FCursorPosition.GetX) + ',' + IntToStr(FCursorPosition.GetY) + ')');
    {$ENDIF}
    
    // Set inline base to current real position - this is where content will start
    FCursorPosition.SetInlineBase(FCursorPosition.GetX, FCursorPosition.GetY);
    FCursorPosition.MarkInlineInitialized;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Set inline base to: (' + IntToStr(FCursorPosition.GetX) + ',' + IntToStr(FCursorPosition.GetY) + ')');
    BDebugLog('  Inline base successfully initialized');
    {$ENDIF}
  end
  else
  begin
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Failed to query real cursor position - using fallback (1,1)');
    {$ENDIF}
    // Fallback: assume we're at (1,1) if cursor query fails
    FCursorPosition.SetInlineBase(1, 1);
    FCursorPosition.MarkInlineInitialized;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Using fallback base (1,1)');
    {$ENDIF}
  end;
end;

procedure TInlineRenderingStrategy.PositionForLine(LineIndex: Integer);
var
  BaseX, BaseY: Integer;
  TargetX, TargetY: Integer;
  TerminalHeight: Integer;
  LinesToCreate: Integer;
  I: Integer;
begin
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('TInlineRenderingStrategy.PositionForLine - Entry');
  BDebugLog('  LineIndex: ' + IntToStr(LineIndex));
  {$ENDIF}
  
  // Get the base position (where content started)
  FCursorPosition.GetInlineBase(BaseX, BaseY);
  TerminalHeight := FDisplay.GetHeight;
  
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('  Retrieved inline base: (' + IntToStr(BaseX) + ',' + IntToStr(BaseY) + ')');
  BDebugLog('  Terminal height: ' + IntToStr(TerminalHeight));
  {$ENDIF}
  
  // Calculate target position: base position + LineIndex down, column 1
  TargetX := 1;
  TargetY := BaseY + LineIndex;
  
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('  Initial calculated target position: (' + IntToStr(TargetX) + ',' + IntToStr(TargetY) + ')');
  {$ENDIF}
  
  // Check if target position exceeds terminal bounds
  if TargetY > TerminalHeight then
  begin
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Target Y (' + IntToStr(TargetY) + ') exceeds terminal height (' + IntToStr(TerminalHeight) + ')');
    {$ENDIF}
    
    // Calculate how many lines we need to create
    LinesToCreate := TargetY - TerminalHeight;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Need to create ' + IntToStr(LinesToCreate) + ' new lines');
    BDebugLog('  Writing newlines to push terminal down...');
    {$ENDIF}
    
    // Write newlines to push the terminal down
    for I := 1 to LinesToCreate do
    begin
      FDisplay.WriteAnsiSequence(LineEnding);
      {$IFDEF BOBAUI_DEBUG}
      BDebugLog('    Wrote newline ' + IntToStr(I) + ' of ' + IntToStr(LinesToCreate));
      {$ENDIF}
    end;
    
    // After scrolling, our base position shifts up by the scroll amount
    BaseY := BaseY - LinesToCreate;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  After scrolling, adjusted base Y from ' + IntToStr(BaseY + LinesToCreate) + ' to ' + IntToStr(BaseY));
    {$ENDIF}
    
    // Update the stored inline base with the new position
    FCursorPosition.SetInlineBase(BaseX, BaseY);
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Updated inline base to: (' + IntToStr(BaseX) + ',' + IntToStr(BaseY) + ')');
    {$ENDIF}
    
    // Recalculate target position with adjusted base
    TargetY := BaseY + LineIndex;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Recalculated target Y after adjustment: ' + IntToStr(TargetY));
    {$ENDIF}
  end
  else
  begin
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Target Y (' + IntToStr(TargetY) + ') is within terminal bounds');
    {$ENDIF}
  end;
  
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('  Final target position: (' + IntToStr(TargetX) + ',' + IntToStr(TargetY) + ')');
  BDebugLog('  About to write ANSI sequence: ' + AnsiMoveCursor(TargetX, TargetY));
  {$ENDIF}
  
  // Now we can safely use absolute positioning (the line exists)
  FDisplay.WriteAnsiSequence(AnsiMoveCursor(TargetX, TargetY));
  
  // Update our tracked position
  FCursorPosition.SetPosition(TargetX, TargetY);
  
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('  Updated tracked position to: (' + IntToStr(TargetX) + ',' + IntToStr(TargetY) + ')');
  BDebugLog('  PositionForLine completed successfully');
  {$ENDIF}
end;

procedure TInlineRenderingStrategy.FinalizeLine;
begin
  // Nothing special needed for inline
end;

procedure TInlineRenderingStrategy.Cleanup;
begin
  // Nothing special needed for inline
end;


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
  Result := (FSequence = ANSI_ARROW_UP) or (FSequence = ANSI_ARROW_DOWN) or 
           (FSequence = ANSI_ARROW_RIGHT) or (FSequence = ANSI_ARROW_LEFT);
end;

function TKeyMsg.IsUpArrow: boolean;
begin
  Result := FSequence = ANSI_ARROW_UP;
end;

function TKeyMsg.IsDownArrow: boolean;
begin
  Result := FSequence = ANSI_ARROW_DOWN;
end;

function TKeyMsg.IsRightArrow: boolean;
begin
  Result := FSequence = ANSI_ARROW_RIGHT;
end;

function TKeyMsg.IsLeftArrow: boolean;
begin
  Result := FSequence = ANSI_ARROW_LEFT;
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

constructor TScheduleTickMsg.Create(const AComponentId: string; AInterval: Integer);
begin
  inherited Create;
  FComponentId := AComponentId;
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

constructor TBatchMsg.Create(const ACommands: array of TCmd);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FCommands, Length(ACommands));
  for i := 0 to High(ACommands) do
    FCommands[i] := ACommands[i];
end;

destructor TBatchMsg.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FCommands) do
    if Assigned(FCommands[i]) then
      FCommands[i].Free;
  SetLength(FCommands, 0);
  inherited Destroy;
end;

function TBatchMsg.GetCommand(Index: Integer): TCmd;
begin
  if (Index >= 0) and (Index <= High(FCommands)) then
    Result := FCommands[Index]
  else
    Result := nil;
end;

function TBatchMsg.GetCommandCount: Integer;
begin
  Result := Length(FCommands);
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

// Command pattern implementations

// Simple commands
function TQuitCommand.Execute: TMsg;
begin
  Result := TQuitMsg.Create;
end;

function TShowCursorCommand.Execute: TMsg;
begin
  Result := TShowCursorMsg.Create;
end;

function THideCursorCommand.Execute: TMsg;
begin
  Result := THideCursorMsg.Create;
end;

// Complex commands
constructor TTickCommand.Create(const AComponentId: string; AInterval: Integer);
begin
  inherited Create;
  FComponentId := AComponentId;
  FInterval := AInterval;
end;

function TTickCommand.Execute: TMsg;
begin
  // Return a scheduling message that the framework will handle
  Result := TScheduleTickMsg.Create(FComponentId, FInterval);
end;

constructor TBatchCommand.Create(const ACommands: array of TCmd);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FCommands, Length(ACommands));
  for i := 0 to High(ACommands) do
    FCommands[i] := ACommands[i];
end;

destructor TBatchCommand.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FCommands) do
    if Assigned(FCommands[i]) then
      FCommands[i].Free;
  SetLength(FCommands, 0);
  inherited Destroy;
end;

function TBatchCommand.Execute: TMsg;
begin
  Result := TBatchMsg.Create(FCommands);
  
  // Don't free sub-commands here - TBatchMsg now owns them
  SetLength(FCommands, 0);
end;

constructor TAddOverlayCommand.Create(AContent: TModel; APosition: TRect; AZIndex: Integer);
begin
  inherited Create;
  FContent := AContent;
  FPosition := APosition;
  FZIndex := AZIndex;
end;

function TAddOverlayCommand.Execute: TMsg;
begin
  Result := TAddOverlayMsg.Create(FContent, FPosition, FZIndex);
end;

function TRemoveTopOverlayCommand.Execute: TMsg;
begin
  Result := TRemoveTopOverlayMsg.Create;
end;

function TClearOverlaysCommand.Execute: TMsg;
begin
  Result := TClearOverlaysMsg.Create;
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
begin
  FCS.Enter;
  try
    // Check if we should drop the message
    if ShouldDropMessage then
    begin
      if Assigned(Msg) then
        Msg.Free;
      Exit;
    end;
    
    // Route message to appropriate queue based on priority
    if IsHighPriorityMessage(Msg) then
      AddToInputQueue(Msg)
    else
      AddToAnimationQueue(Msg);
  finally
    FCS.Leave;
  end;
end;

function TMessageQueue.ShouldDropMessage: Boolean;
begin
  Result := FDestroying;
end;

function TMessageQueue.IsHighPriorityMessage(const Msg: TMsg): Boolean;
begin
  Result := (Msg is TKeyMsg) or (Msg is TWindowSizeMsg) or (Msg is TQuitMsg);
end;

procedure TMessageQueue.ResizeInputQueueIfNeeded;
begin
  if FInputCount >= Length(FInputMsgObjects) then
    SetLength(FInputMsgObjects, FInputCount + 10);
end;

procedure TMessageQueue.AddToInputQueue(const Msg: TMsg);
begin
  ResizeInputQueueIfNeeded;
  FInputMsgObjects[FInputCount] := Msg;
  Inc(FInputCount);
end;

procedure TMessageQueue.TrimAnimationQueue;
var
  i: integer;
begin
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
end;

procedure TMessageQueue.ResizeAnimationQueueIfNeeded;
begin
  if FAnimationCount >= Length(FAnimationMsgObjects) then
    SetLength(FAnimationMsgObjects, FAnimationCount + 10);
end;

procedure TMessageQueue.AddToAnimationQueue(const Msg: TMsg);
begin
  TrimAnimationQueue;
  ResizeAnimationQueueIfNeeded;
  FAnimationMsgObjects[FAnimationCount] := Msg;
  Inc(FAnimationCount);
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
      EscapeSeq := ANSI_ESC_PREFIX;
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
      EscapeSeq := ANSI_ESC_PREFIX;
      while (I <= Length(Result)) and (Result[I] <> 'm') do
      begin
        EscapeSeq := EscapeSeq + Result[I];
        Inc(I);
      end;
      
      if (I <= Length(Result)) and (Result[I] = 'm') then
      begin
        EscapeSeq := EscapeSeq + 'm';
        
        // Check if this is a reset sequence
        if (EscapeSeq = ANSI_RESET_ALL) or (EscapeSeq = ANSI_RESET_SHORT) or
           (EscapeSeq = ANSI_RESET_FOREGROUND) or (EscapeSeq = ANSI_RESET_BACKGROUND) or
           (EscapeSeq = ANSI_RESET_FG_BG) or (EscapeSeq = ANSI_RESET_BG_FG) then
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
begin
  Result := False;
  
  // Validate overlay
  if not ValidateOverlay(Overlay) then
    Exit;

  try
    // Prepare overlay data
    PrepareOverlayData(Overlay, OverlayLines, OverlayX, OverlayY);

    // Composite each line of the overlay onto the base
    for I := 0 to Length(OverlayLines) - 1 do
    begin
      TargetY := OverlayY + I;
      CompositeOverlayLine(BaseLines, OverlayLines[I], TargetY, OverlayX);
    end;
    
    Result := True;
  except
    // Silently handle any compositing errors
    Result := False;
  end;
end;

function TModel.ValidateOverlay(const Overlay: TOverlay): Boolean;
begin
  Result := Assigned(Overlay) and Overlay.Visible and Assigned(Overlay.Content);
end;

procedure TModel.PrepareOverlayData(const Overlay: TOverlay; out OverlayLines: TStringArray; out OverlayX, OverlayY: Integer);
begin
  OverlayLines := bobastyle.SplitLines(Overlay.Content.View);
  OverlayX := Overlay.Position.X;
  OverlayY := Overlay.Position.Y;
end;

function TModel.PrepareBaseLineForOverlay(const BaseLine: string; OverlayX: Integer): string;
begin
  Result := BaseLine;
  
  // If overlay starts beyond the base line, extend the base line
  while Utf8DisplayWidth(Result) < OverlayX - 1 do
    Result := Result + ' ';
end;

function TModel.SplitBaseLineForOverlay(const BaseLine: string; OverlayX: Integer; OverlayWidth: Integer; out BeforePart, AfterPart: string): Boolean;
var
  DummyPart: string;
  TempAfterPart: string;
begin
  Result := False;
  
  // Split base line into before and after parts using display-aware splitting
  if not SplitAnsiStringAtDisplay(BaseLine, OverlayX - 1, BeforePart, AfterPart) then
  begin
    // Fallback if split failed
    BeforePart := BaseLine;
    AfterPart := '';
    Exit;
  end;
  
  // Now split the after part to account for the overlay width
  if OverlayWidth > 0 then
  begin
    // We need to skip the part that will be replaced by the overlay
    // and keep everything after that
    TempAfterPart := AfterPart;
    if not SplitAnsiStringAtDisplay(TempAfterPart, OverlayWidth, DummyPart, AfterPart) then
    begin
      // If split failed, keep the entire AfterPart
      // (don't set it to empty string)
    end;
    // If split succeeded, DummyPart contains the replaced text (discarded)
    // and AfterPart contains the text after the overlay
  end;
  
  Result := True;
end;

function TModel.RestoreColorsAfterOverlay(const AfterPart, ActiveBgColor, ActiveFgColor: string): string;
begin
  Result := AfterPart;
  
  // Restore the original background and foreground colors after the overlay
  if Length(Result) > 0 then
  begin
    // Apply both background and foreground colors if available
    if (Length(ActiveBgColor) > 0) and (Length(ActiveFgColor) > 0) then
      Result := ActiveBgColor + ActiveFgColor + Result
    else if Length(ActiveBgColor) > 0 then
      Result := ActiveBgColor + Result
    else if Length(ActiveFgColor) > 0 then
      Result := ActiveFgColor + Result;
  end;
end;

function TModel.CompositeOverlayLine(var BaseLines: TStringArray; const OverlayLine: string; TargetY, OverlayX: Integer): Boolean;
var
  BaseLine: string;
  BeforePart, AfterPart: string;
  ActiveBgColor: string;
  ActiveFgColor: string;
  PreparedOverlayLine: string;
  PreparedBaseLine: string;
begin
  Result := False;
  
  // Skip if overlay line is outside base content bounds
  if (TargetY < 1) or (TargetY > Length(BaseLines)) then
    Exit;
    
  if Length(OverlayLine) = 0 then
    Exit;
  
  // Strip ALL reset sequences to prevent background color bleeding
  PreparedOverlayLine := StripAllResets(OverlayLine);
  
  // Adjust for 0-based array indexing
  BaseLine := BaseLines[TargetY - 1];
  
  // Extract both background and foreground colors from original base line BEFORE any modifications
  ExtractActiveColors(BaseLine, ActiveBgColor, ActiveFgColor);
  
  // Prepare base line for overlay
  PreparedBaseLine := PrepareBaseLineForOverlay(BaseLine, OverlayX);
  
  // Split base line around overlay position
  if not SplitBaseLineForOverlay(PreparedBaseLine, OverlayX, Utf8DisplayWidth(PreparedOverlayLine), BeforePart, AfterPart) then
    Exit;
  
  // Restore colors after overlay
  AfterPart := RestoreColorsAfterOverlay(AfterPart, ActiveBgColor, ActiveFgColor);
  
  // Combine the parts
  BaseLines[TargetY - 1] := BeforePart + PreparedOverlayLine + AfterPart;
  
  Result := True;
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
  
  // Create component instances
  FTerminalManager := TTerminalManager.Create;
  FAnsiRenderer := TAnsiRenderer.Create;
  
  // Initialize cursor position manager
  FCursorPosition := TCursorPosition.Create(AMode, Self);
  
  // Create appropriate rendering strategy
  if FDisplayMode = dmFullScreen then
  begin
    FRenderingStrategy := TFullscreenRenderingStrategy.Create(Self, FCursorPosition);
    // Switch to alternate screen buffer
    FAnsiRenderer.WriteAnsiSequence(ANSI_ALT_SCREEN_ON);
    // Hide cursor by default (like BubbleTea)
    FAnsiRenderer.WriteAnsiSequence(ANSI_CURSOR_HIDE);
  end
  else
  begin
    FRenderingStrategy := TInlineRenderingStrategy.Create(Self, FCursorPosition);
    // For inline mode (default), preserve existing terminal content
  end;
  
  // Create differential renderer with dependencies
  FDifferentialRenderer := TDifferentialRenderer.Create(FAnsiRenderer, FRenderingStrategy);
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
    FAnsiRenderer.WriteAnsiSequence(ANSI_CURSOR_SHOW);
    // Switch back to main screen buffer
    FAnsiRenderer.WriteAnsiSequence(ANSI_ALT_SCREEN_OFF);
  end;
  
  // Clean up component instances
  if Assigned(FDifferentialRenderer) then
    FDifferentialRenderer.Free;
  if Assigned(FRenderingStrategy) then
    FRenderingStrategy.Free;
  if Assigned(FAnsiRenderer) then
    FAnsiRenderer.Free;
  if Assigned(FTerminalManager) then
    FTerminalManager.Free;
  if Assigned(FCursorPosition) then
    FCursorPosition.Free;
  inherited Destroy;
end;

// TTerminalManager implementation

constructor TTerminalManager.Create;
begin
  inherited Create;
  DetectTerminalSize;
  SetRawMode;
end;

destructor TTerminalManager.Destroy;
begin
  RestoreTerminal;
  inherited Destroy;
end;

procedure TTerminalManager.DetectTerminalSize;
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
  FWidth := 80;
  FHeight := 24;
  
  {$IFDEF UNIX}
  // Get actual terminal size using ioctl
  if fpioctl(STDOUT_FILENO, TIOCGWINSZ, @WS) = 0 then
  begin
    if (WS.ws_col > 0) and (WS.ws_row > 0) then
    begin
      FWidth := WS.ws_col;
      FHeight := WS.ws_row;
    end;
  end;
  {$ENDIF}
end;

procedure TTerminalManager.SetRawMode;
{$IFDEF UNIX}
var
  NewTermios: TermIOs;
{$ENDIF}
begin
  {$IFDEF UNIX}
  // Save original terminal settings
  TCGetAttr(STDIN_FILENO, FOriginalTermios);
  
  // Set up new terminal settings for raw mode
  NewTermios := FOriginalTermios;
  
  // Disable canonical mode, echo, and signals
  NewTermios.c_lflag := NewTermios.c_lflag and not (ICANON or ECHO or ISIG);
  
  // Set minimum characters to read and timeout
  NewTermios.c_cc[VMIN] := 1;   // Read at least 1 character
  NewTermios.c_cc[VTIME] := 0;  // No timeout
  
  // Apply the new settings
  TCSetAttr(STDIN_FILENO, TCSANOW, NewTermios);
  {$ENDIF}
end;

procedure TTerminalManager.RestoreTerminal;
begin
  {$IFDEF UNIX}
  // Restore original terminal settings
  TCSetAttr(STDIN_FILENO, TCSANOW, FOriginalTermios);
  {$ENDIF}
end;

function TTerminalManager.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TTerminalManager.GetHeight: Integer;
begin
  Result := FHeight;
end;

// TAnsiRenderer implementation

procedure TAnsiRenderer.WriteAnsiSequence(const Sequence: string);
begin
  // Write ANSI escape sequence directly
  system.write(Sequence);
  system.flush(output);
end;

procedure TAnsiRenderer.MoveCursor(X, Y: Integer);
begin
  WriteAnsiSequence(AnsiMoveCursor(X, Y));
end;

procedure TAnsiRenderer.ClearToEndOfLine;
begin
  WriteAnsiSequence(ANSI_CLEAR_TO_EOL);
end;

procedure TAnsiRenderer.ClearFromCursor;
begin
  WriteAnsiSequence(ANSI_CLEAR_FROM_CURSOR);
end;

// TDifferentialRenderer implementation

constructor TDifferentialRenderer.Create(AAnsiRenderer: TAnsiRenderer; ARenderingStrategy: TRenderingStrategy);
begin
  inherited Create;
  FAnsiRenderer := AAnsiRenderer;
  FRenderingStrategy := ARenderingStrategy;
  SetLength(FLastRender, 0);
  FNeedsFullRedraw := True;
end;

destructor TDifferentialRenderer.Destroy;
begin
  // Note: FAnsiRenderer and FRenderingStrategy are owned by TAnsiDisplay
  SetLength(FLastRender, 0);
  inherited Destroy;
end;

procedure TDifferentialRenderer.ForceFullRedraw;
begin
  FNeedsFullRedraw := True;
end;

procedure TDifferentialRenderer.DifferentialWrite(const Content: string);
var
  NewLines: TStringArray;
begin
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('TDifferentialRenderer.DifferentialWrite - Entry');
  BDebugLog('  Content length: ' + IntToStr(Length(Content)) + ' bytes');
  {$ENDIF}
  
  // Split content into lines using the function from bobastyle
  NewLines := bobastyle.SplitLines(Content);
  
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('  Line count: ' + IntToStr(Length(NewLines)));
  BDebugLog('  FNeedsFullRedraw: ' + BoolToStr(FNeedsFullRedraw, True));
  BDebugLog('  Length(FLastRender): ' + IntToStr(Length(FLastRender)));
  {$ENDIF}
  
  
  // Handle first render or full redraw
  if FNeedsFullRedraw or (Length(FLastRender) = 0) then
  begin
    HandleFirstRender(NewLines);
    Exit;
  end;
  
  // Differential rendering
  UpdateChangedLines(NewLines);
  ClearRemainingLines(NewLines);
  
  // Update our last render buffer
  FLastRender := Copy(NewLines);
  system.flush(output);
end;

function TDifferentialRenderer.ShouldUpdateLine(LineIndex: Integer; const NewLine: string): Boolean;
begin
  Result := False;
  
  // New line beyond previous content
  if LineIndex >= Length(FLastRender) then
  begin
    Result := True;
    Exit;
  end;
  
  // Line content changed
  if NewLine <> FLastRender[LineIndex] then
    Result := True;
end;

procedure TDifferentialRenderer.HandleFirstRender(const NewLines: TStringArray);
var
  I: Integer;
begin
  {$IFDEF BOBAUI_DEBUG}
  BDebugLog('TDifferentialRenderer.HandleFirstRender - Entry');
  BDebugLog('  Line count: ' + IntToStr(Length(NewLines)));
  {$ENDIF}
  
  // Use strategy for first render initialization
  FRenderingStrategy.InitializeFirstRender;
  
  // Write all lines using strategy positioning
  for I := 0 to High(NewLines) do
  begin
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Processing Line ' + IntToStr(I) + ': "' + Copy(NewLines[I], 1, 50) + '"...');
    {$ENDIF}
    
    // Use strategy to position for each line
    FRenderingStrategy.PositionForLine(I);
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  About to write line content: "' + Copy(NewLines[I], 1, 30) + '"');
    {$ENDIF}
    
    system.write(NewLines[I]);
    FRenderingStrategy.FinalizeLine;
    
    {$IFDEF BOBAUI_DEBUG}
    BDebugLog('  Finished writing line ' + IntToStr(I));
    {$ENDIF}
  end;
  
  // Update state
  FLastRender := Copy(NewLines);
  FNeedsFullRedraw := False;
  
  
  system.flush(output);
end;

procedure TDifferentialRenderer.UpdateChangedLines(const NewLines: TStringArray);
var
  I, MaxLines: Integer;
  Line: string;
begin
  // Compare line by line up to the maximum of old or new content
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
    
    // Update line if needed
    if ShouldUpdateLine(I, Line) then
    begin
      // Use strategy for line positioning
      FRenderingStrategy.PositionForLine(I);
      
      // Write the new line content
      system.write(Line);
      
      // Always clear to end of line when content changes to handle ANSI styled content properly
      // The original logic only cleared when new line was shorter, but ANSI codes make 
      // byte length comparison unreliable for determining visual width changes
      if (I < Length(FLastRender)) then
        FAnsiRenderer.ClearToEndOfLine;
    end;
  end;
end;

procedure TDifferentialRenderer.ClearRemainingLines(const NewLines: TStringArray);
var
  I: Integer;
begin
  // If new content has fewer lines, clear the remaining old lines
  if Length(FLastRender) > Length(NewLines) then
  begin
    for I := Length(NewLines) to Length(FLastRender) - 1 do
    begin
      // Use strategy for line positioning
      FRenderingStrategy.PositionForLine(I);
      FAnsiRenderer.ClearToEndOfLine;
    end;
  end;
end;

// TAnsiDisplay method implementations using component delegation

procedure TAnsiDisplay.WriteAnsiSequence(const Sequence: string);
begin
  // Delegate to ANSI renderer component
  FAnsiRenderer.WriteAnsiSequence(Sequence);
end;

procedure TAnsiDisplay.Clear;
begin
  // Delegate to differential renderer component
  FDifferentialRenderer.ForceFullRedraw;
end;

procedure TAnsiDisplay.Write(const S: string);
begin
  // Delegate to differential renderer component
  FDifferentialRenderer.DifferentialWrite(S);
end;

procedure TAnsiDisplay.WriteLn(const S: string);
begin
  // Direct WriteLn - UTF-8 safe
  system.writeln(S);
end;

procedure TAnsiDisplay.MoveCursor(X, Y: integer);
var
  CurrentX, CurrentY: Integer;
begin
  FCursorPosition.GetPosition(CurrentX, CurrentY);
  // Only move cursor if it's not already at the target position
  if (CurrentX <> X) or (CurrentY <> Y) then
  begin
    // Delegate to ANSI renderer component
    FAnsiRenderer.MoveCursor(X, Y);
    // Update tracked cursor position
    FCursorPosition.SetPosition(X, Y);
  end;
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
  // Delegate to terminal manager component
  Result := FTerminalManager.GetWidth;
end;

function TAnsiDisplay.GetHeight: integer;
begin
  // Delegate to terminal manager component
  Result := FTerminalManager.GetHeight;
end;

procedure TAnsiDisplay.ClearToEndOfLine;
begin
  // Delegate to ANSI renderer component
  FAnsiRenderer.ClearToEndOfLine;
end;

procedure TAnsiDisplay.ClearFromCursor;
begin
  // Delegate to ANSI renderer component
  FAnsiRenderer.ClearFromCursor;
end;

procedure TAnsiDisplay.ForceFullRedraw;
begin
  // Delegate to differential renderer component
  FDifferentialRenderer.ForceFullRedraw;
end;

function TAnsiDisplay.GetDisplayMode: TDisplayMode;
begin
  Result := FDisplayMode;
end;

procedure TAnsiDisplay.SetDisplayMode(AMode: TDisplayMode);
begin
  if FDisplayMode <> AMode then
  begin
    FDisplayMode := AMode;
    // Update the rendering strategy
    FRenderingStrategy.Free;
    if FDisplayMode = dmInline then
      FRenderingStrategy := TInlineRenderingStrategy.Create(Self, FCursorPosition)
    else
      FRenderingStrategy := TFullscreenRenderingStrategy.Create(Self, FCursorPosition);
  end;
end;

// Old duplicate methods removed - functionality moved to component classes

// Removed old methods - functionality moved to TDifferentialRenderer component

// TInputParser implementation - separates parsing logic from threading concerns
constructor TInputParser.Create(ADisplay: TDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  FHasPendingChar := False;
  FPendingChar := #0;
end;

function TInputParser.ReadNextChar: char;
begin
  Result := FDisplay.ReadKey;
end;

function TInputParser.ParseArrowSequence(FirstChar: char): char;
begin
  if FirstChar = '[' then
  begin
    Result := ReadNextChar; // Should be A, B, C, or D
  end
  else
  begin
    Result := FirstChar; // Not a valid arrow sequence
  end;
end;

function TInputParser.CreateArrowKeyMessage(ArrowChar: char): TKeyMsg;
begin
  case ArrowChar of
    'A': Result := TKeyMsg.Create(ANSI_ARROW_UP);    // Up arrow
    'B': Result := TKeyMsg.Create(ANSI_ARROW_DOWN);  // Down arrow  
    'C': Result := TKeyMsg.Create(ANSI_ARROW_RIGHT); // Right arrow
    'D': Result := TKeyMsg.Create(ANSI_ARROW_LEFT);  // Left arrow
    else
      Result := TKeyMsg.Create(#27); // Invalid arrow sequence, treat as ESC
  end;
end;

function TInputParser.ParseInput(InitialChar: char): TKeyMsg;
var
  SecondChar, ThirdChar: char;
begin
  // Handle escape sequences (arrow keys)
  if InitialChar = #27 then  // ESC character
  begin
    SecondChar := ReadNextChar;  // Should be '['
    ThirdChar := ParseArrowSequence(SecondChar);
    
    if (SecondChar = '[') and (ThirdChar in ['A', 'B', 'C', 'D']) then
    begin
      // Valid arrow key sequence
      Result := CreateArrowKeyMessage(ThirdChar);
    end
    else
    begin
      // Invalid sequence - store the second character for later retrieval
      FPendingChar := SecondChar;
      FHasPendingChar := True;
      Result := TKeyMsg.Create(#27);
    end;
  end
  else
  begin
    // Regular character
    Result := TKeyMsg.Create(InitialChar);
  end;
end;

function TInputParser.HasPendingInput: Boolean;
begin
  Result := FHasPendingChar;
end;

function TInputParser.GetPendingInput: TKeyMsg;
begin
  if FHasPendingChar then
  begin
    Result := TKeyMsg.Create(FPendingChar);
    FHasPendingChar := False;
    FPendingChar := #0;
  end
  else
  begin
    Result := nil;
  end;
end;

constructor TInputThread.Create(AMessageQueue: TMessageQueue; ADisplay: TDisplay);
begin
  inherited Create(True); // Create suspended
  FMessageQueue := AMessageQueue;
  FDisplay := ADisplay;
  FInputParser := TInputParser.Create(ADisplay);
  FreeOnTerminate := False;
end;

destructor TInputThread.Destroy;
begin
  FInputParser.Free;
  inherited Destroy;
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
  KeyMsg: TKeyMsg;
begin
  while not Terminated do
  begin
    try
      // First check for pending input from parser
      if Assigned(FMessageQueue) and FInputParser.HasPendingInput then
      begin
        KeyMsg := FInputParser.GetPendingInput;
        if Assigned(KeyMsg) then
          FMessageQueue.Push(KeyMsg);
      end
      else if Assigned(FMessageQueue) and Assigned(FDisplay) and FDisplay.KeyPressed then
      begin
        Ch := FDisplay.ReadKey;
        if Assigned(FMessageQueue) then // Check again in case it was freed
        begin
          // Use the input parser to handle escape sequences
          KeyMsg := FInputParser.ParseInput(Ch);
          FMessageQueue.Push(KeyMsg);
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
  RegisterProgram(Self);
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
  RegisterProgram(Self);
  InitializeSignalHandlers;
  {$ENDIF}
end;

destructor TBobaUIProgram.Destroy;
begin
  try
    // Set running to false first to stop main loop
    FRunning := false;
    
    {$IFDEF UNIX}
    UnregisterProgram(Self);
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

procedure TBobaUIProgram.SetDisplayMode(AMode: TDisplayMode);
begin
  if FDisplay is TAnsiDisplay then
    TAnsiDisplay(FDisplay).SetDisplayMode(AMode);
end;

// ANSI sequence helper function implementations

function AnsiMoveCursor(X, Y: Integer): string;
begin
  Result := ANSI_ESC_PREFIX + IntToStr(Y) + ';' + IntToStr(X) + 'H';
end;

function AnsiMoveDown(Lines: Integer): string;
begin
  Result := ANSI_ESC_PREFIX + IntToStr(Lines) + 'B';
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

procedure TBobaUIProgram.Send(const Msg: TMsg);
begin
  if Assigned(FMessageQueue) then
    FMessageQueue.Push(Msg);
end;

// Old command implementations removed - now using command pattern

function QuitCmd: TCmd;
begin
  Result := TQuitCommand.Create;
end;

function ShowCursorCmd: TCmd;
begin
  Result := TShowCursorCommand.Create;
end;

function HideCursorCmd: TCmd;
begin
  Result := THideCursorCommand.Create;
end;

function BatchCmd(const Commands: array of TCmd): TCmd;
var
  ValidCommands: array of TCmd;
  i, ValidCount: Integer;
begin
  // Filter out nil commands
  ValidCount := 0;
  SetLength(ValidCommands, Length(Commands));
  for i := 0 to High(Commands) do
  begin
    if Assigned(Commands[i]) then
    begin
      ValidCommands[ValidCount] := Commands[i];
      Inc(ValidCount);
    end;
  end;
  SetLength(ValidCommands, ValidCount);
  
  // Return appropriate result based on valid command count
  case ValidCount of
    0: Result := nil;
    1: Result := ValidCommands[0];
    else Result := TBatchCommand.Create(ValidCommands);
  end;
end;

function TickCmd(const ComponentId: string; Interval: integer): TCmd;
begin
  Result := TTickCommand.Create(ComponentId, Interval);
end;

// Old overlay command implementations removed - now using command pattern

function AddOverlayCmd(Content: TModel; Position: TRect; ZIndex: Integer): TCmd;
begin
  Result := TAddOverlayCommand.Create(Content, Position, ZIndex);
end;

function RemoveTopOverlayCmd: TCmd;
begin
  Result := TRemoveTopOverlayCommand.Create;
end;

function ClearOverlaysCmd: TCmd;
begin
  Result := TClearOverlaysCommand.Create;
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
  I: Integer;
begin
  // Signal handler - must be minimal and signal-safe
  if Assigned(ActiveProgramsCS) then
  begin
    GetCurrentTerminalSize(Width, Height);
    
    ActiveProgramsCS.Enter;
    try
      // Notify all active program instances
      for I := 0 to Length(ActivePrograms) - 1 do
      begin
        if Assigned(ActivePrograms[I]) then
          ActivePrograms[I].HandleWindowResize(Width, Height);
      end;
    finally
      ActiveProgramsCS.Leave;
    end;
  end;
end;

// Program registration functions for signal handling
procedure RegisterProgram(AProgram: TBobaUIProgram);
var
  NewLength: Integer;
begin
  if not Assigned(ActiveProgramsCS) then
    ActiveProgramsCS := TCriticalSection.Create;
    
  ActiveProgramsCS.Enter;
  try
    NewLength := Length(ActivePrograms);
    SetLength(ActivePrograms, NewLength + 1);
    ActivePrograms[NewLength] := AProgram;
  finally
    ActiveProgramsCS.Leave;
  end;
end;

procedure UnregisterProgram(AProgram: TBobaUIProgram);
var
  I, J: Integer;
begin
  if not Assigned(ActiveProgramsCS) then
    Exit;
    
  ActiveProgramsCS.Enter;
  try
    for I := 0 to Length(ActivePrograms) - 1 do
    begin
      if ActivePrograms[I] = AProgram then
      begin
        // Remove by shifting remaining elements
        for J := I to Length(ActivePrograms) - 2 do
          ActivePrograms[J] := ActivePrograms[J + 1];
        SetLength(ActivePrograms, Length(ActivePrograms) - 1);
        Break;
      end;
    end;
  finally
    ActiveProgramsCS.Leave;
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
  BatchMsg: TBatchMsg;
  I: Integer;
  BatchCmdMsg: TMsg;
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
          CmdMsg := UpdateResult.Cmd.Execute;
          UpdateResult.Cmd.Free; // Free the command object after execution
          if Assigned(CmdMsg) then
          begin
            if CmdMsg is TQuitMsg then
            begin
              Quit;
              CmdMsg.Free;
            end
            else if CmdMsg is TShowCursorMsg then
            begin
              if FDisplay is TAnsiDisplay then
                TAnsiDisplay(FDisplay).WriteAnsiSequence(ANSI_CURSOR_SHOW);
              CmdMsg.Free;
            end
            else if CmdMsg is THideCursorMsg then
            begin
              if FDisplay is TAnsiDisplay then
                TAnsiDisplay(FDisplay).WriteAnsiSequence(ANSI_CURSOR_HIDE);
              CmdMsg.Free;
            end
            else if CmdMsg is TScheduleTickMsg then
            begin
              // Handle tick scheduling - call the method that was previously called via global
              ScheduleComponentTick(TScheduleTickMsg(CmdMsg).ComponentId, TScheduleTickMsg(CmdMsg).Interval);
              CmdMsg.Free;
            end
            else if CmdMsg is TBatchMsg then
            begin
              // Execute all commands in the batch sequentially
              BatchMsg := TBatchMsg(CmdMsg);
              for I := 0 to BatchMsg.CommandCount - 1 do
              begin
                if Assigned(BatchMsg.Commands[I]) then
                begin
                  BatchCmdMsg := BatchMsg.Commands[I].Execute;
                  if Assigned(BatchCmdMsg) then
                  begin
                    // Handle the sub-command message immediately instead of queuing
                    if BatchCmdMsg is TQuitMsg then
                    begin
                      Quit;
                      BatchCmdMsg.Free;
                    end
                    else if BatchCmdMsg is TShowCursorMsg then
                    begin
                      if FDisplay is TAnsiDisplay then
                        TAnsiDisplay(FDisplay).WriteAnsiSequence(ANSI_CURSOR_SHOW);
                      BatchCmdMsg.Free;
                    end
                    else if BatchCmdMsg is THideCursorMsg then
                    begin
                      if FDisplay is TAnsiDisplay then
                        TAnsiDisplay(FDisplay).WriteAnsiSequence(ANSI_CURSOR_HIDE);
                      BatchCmdMsg.Free;
                    end
                    else if BatchCmdMsg is TScheduleTickMsg then
                    begin
                      // Handle tick scheduling
                      ScheduleComponentTick(TScheduleTickMsg(BatchCmdMsg).ComponentId, TScheduleTickMsg(BatchCmdMsg).Interval);
                      BatchCmdMsg.Free;
                    end
                    else
                    begin
                      // For other message types, push to queue
                      FMessageQueue.Push(BatchCmdMsg);
                    end;
                  end;
                end;
              end;
              CmdMsg.Free;
            end
            else
            begin
              // For user-defined messages, push them back to the queue
              // so they get processed in the next Update cycle
              FMessageQueue.Push(CmdMsg);
              CmdMsg := nil; // Don't free - queue owns it now
            end;
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

{$IFDEF BOBAUI_DEBUG}
finalization
  CloseBDebugLog;
{$ENDIF}

end. 
