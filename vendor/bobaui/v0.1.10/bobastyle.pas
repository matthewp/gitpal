{$codepage UTF8}
unit bobastyle;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Math, bobaflow;

type
  // Color definitions for terminal text
  TColor = (
    cDefault,    // Default terminal color
    cBlack,      // Black
    cRed,        // Red
    cGreen,      // Green
    cYellow,     // Yellow
    cBlue,       // Blue
    cMagenta,    // Magenta
    cCyan,       // Cyan
    cWhite,      // White
    cBrightBlack,   // Bright Black (Gray)
    cBrightRed,     // Bright Red
    cBrightGreen,   // Bright Green
    cBrightYellow,  // Bright Yellow
    cBrightBlue,    // Bright Blue
    cBrightMagenta, // Bright Magenta
    cBrightCyan,    // Bright Cyan
    cBrightWhite    // Bright White
  );

  // RGB Color record for true color support
  TRGBColor = record
    R, G, B: Byte;
  end;

  // Border style type
  TBorderStyle = (
    bsNone,      // No border
    bsSingle,    // Single line border
    bsDouble,    // Double line border
    bsRounded,   // Rounded corners
    bsBold,      // Bold single line
    bsRadius     // Rounded corners with a specific radius
  );

  TTitlePosition = (tpLeft, tpCenter, tpRight);

  // Import types from bobaflow
  TTextWrapping = bobaflow.TTextWrapping;
  TFrameSize = bobaflow.TFrameSize;
  TAnsiState = bobaflow.TAnsiState;
  TStringArray = bobaflow.TStringArray;

  // Border characters for different styles (now string, not char)
  TBorderChars = record
    TopLeft, TopRight, BottomLeft, BottomRight: string;
    Horizontal, Vertical: string;
  end;

  // Style class for managing text and border styling
  TStyle = class
  private
    FBorderStyle: TBorderStyle;
    FBorderChars: TBorderChars;
    FBorderColor: TColor;
    FBorderBackgroundColor: TColor;
    FBorderBackgroundColorRGB: TRGBColor;
    FUseRGBBorderBackground: Boolean;
    FBorderRadius: integer;
    FTitle: AnsiString;
    FTitlePosition: TTitlePosition;
    FTitleColor: TColor;
    FWidth: integer;
    FHeight: integer;
    FContent: string;
    
    // Background and content color support
    FBackgroundColor: TColor;
    FBackgroundColorRGB: TRGBColor;
    FUseRGBBackground: Boolean;
    FContentColor: TColor;
    FContentColorRGB: TRGBColor;
    FUseRGBContent: Boolean;
    
    // Text wrapping support
    FTextWrapping: TTextWrapping;
    FWordWrapIndent: integer;
    
    procedure SetBorderStyle(AStyle: TBorderStyle);
    function GetBorderChars: TBorderChars;
  public
    constructor Create;
    
    // Border style management
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderBackgroundColor: TColor read FBorderBackgroundColor write FBorderBackgroundColor;
    property BorderBackgroundColorRGB: TRGBColor read FBorderBackgroundColorRGB write FBorderBackgroundColorRGB;
    property UseRGBBorderBackground: Boolean read FUseRGBBorderBackground write FUseRGBBorderBackground;
    property BorderRadius: integer read FBorderRadius write FBorderRadius;
    property Title: AnsiString read FTitle write FTitle;
    property TitlePosition: TTitlePosition read FTitlePosition write FTitlePosition;
    property TitleColor: TColor read FTitleColor write FTitleColor;
    
    // Dimensions
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    
    // Content
    property Content: string read FContent write FContent;
    
    // Background and content color properties
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BackgroundColorRGB: TRGBColor read FBackgroundColorRGB write FBackgroundColorRGB;
    property UseRGBBackground: Boolean read FUseRGBBackground write FUseRGBBackground;
    property ContentColor: TColor read FContentColor write FContentColor;
    property ContentColorRGB: TRGBColor read FContentColorRGB write FContentColorRGB;
    property UseRGBContent: Boolean read FUseRGBContent write FUseRGBContent;
    
    // Text wrapping properties
    property TextWrapping: TTextWrapping read FTextWrapping write FTextWrapping;
    property WordWrapIndent: integer read FWordWrapIndent write FWordWrapIndent;
    
    // Rendering
    function Render: string;
  end;

const
  // Import text wrapping constants from bobaflow
  twNone = bobaflow.twNone;
  twWord = bobaflow.twWord;
  twChar = bobaflow.twChar;
  twAuto = bobaflow.twAuto;

// Border style functions (ensure AnsiString types)
function GetSingleBorder: TBorderChars;
function GetDoubleBorder: TBorderChars;
function GetRoundedBorder: TBorderChars;
function GetBoldBorder: TBorderChars;

function SplitLines(const S: string): TStringArray;
function StripAnsiEscapes(const S: string): string;
function Utf8CodepointCount(const S: string): integer;
function Utf8DisplayWidth(const S: string): integer;
function TruncateAnsiStringAtDisplay(const S: string; MaxWidth: integer): string;
function SplitAnsiStringAtDisplay(const S: string; SplitPos: integer; out BeforePart, AfterPart: string): Boolean;
function JoinHorizontal(const Left, Right: string; Separator: string = ' '): string;
function JoinVertical(const Blocks: array of string): string; overload;
function JoinVertical(const Blocks: TStringArray): string; overload;

// Color utility functions
function GetColorCode(Color: TColor): string;
function ColorText(const Text: string; Color: TColor): string;
function ResetColor: string;

// RGB Color utility functions
function RGB(R, G, B: Byte): TRGBColor;
function GetRGBColorCode(const Color: TRGBColor; Background: Boolean = False): string;
function ColorTextRGB(const Text: string; const Color: TRGBColor): string;
function ColorTextRGBBg(const Text: string; const FgColor, BgColor: TRGBColor): string;

// Gradient and animation functions
function HSVtoRGB(H, S, V: Double): TRGBColor;
function InterpolateRGB(const Color1, Color2: TRGBColor; t: Double): TRGBColor;
function GetRainbowColor(Position: Double): TRGBColor; // Position 0.0-1.0
function CreateGradientLine(const Text: string; StartHue, EndHue: Double): string;

// Background styling convenience functions
function CreateBackgroundStyle(Width, Height: Integer; BgColor: TRGBColor): TStyle;
function CreateColoredStyle(Width, Height: Integer; FgColor, BgColor: TRGBColor): TStyle;
function PadWithBackground(const Content: string; Width, Height: Integer; BgColor: TRGBColor): string;
function FillArea(Width, Height: Integer; BgColor: TRGBColor): string;
function ApplyBackgroundToText(const Text: string; BgColor: TRGBColor): string;
function ColorBorderText(const Text: string; FgColor: TColor; BgColor: TColor; BgColorRGB: TRGBColor; UseRGBBg: Boolean): string;

// Text wrapping functions
function WrapText(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
function WrapTextAnsiAware(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
function WrapTextHard(const Text: string; MaxWidth: Integer): TStringArray;
function NeedsWrapping(const Text: string; MaxWidth: Integer): Boolean;
function IsWordBreakCharacter(C: Char): Boolean;
function FindWrapPosition(const Line: string; MaxWidth: Integer; Mode: TTextWrapping): Integer;

// ANSI sequence handling
function ExtractAnsiState(const Text: string): TAnsiState;
function ApplyAnsiState(const Text: string; const State: TAnsiState): string;
function CarryForwardAnsiState(const PreviousLine, NextLine: string): string;
function SplitTextPreservingAnsi(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;

// Frame size calculations
function GetFrameSize(BorderStyle: TBorderStyle; HasTitle: Boolean = False): TFrameSize;
function GetContentWidth(TotalWidth: Integer; BorderStyle: TBorderStyle): Integer;
function GetContentHeight(TotalHeight: Integer; BorderStyle: TBorderStyle; HasTitle: Boolean = False): Integer;

// Utility functions for better text handling
function TrimTrailingSpaces(const S: string): string;
function CountLeadingSpaces(const S: string): Integer;
function PreserveIndentation(const OriginalLine, WrappedLine: string): string;

implementation

// Color utility functions
function GetColorCode(Color: TColor): string;
begin
  case Color of
    cDefault: Result := '';
    cBlack: Result := #27'[30m';
    cRed: Result := #27'[31m';
    cGreen: Result := #27'[32m';
    cYellow: Result := #27'[33m';
    cBlue: Result := #27'[34m';
    cMagenta: Result := #27'[35m';
    cCyan: Result := #27'[36m';
    cWhite: Result := #27'[37m';
    cBrightBlack: Result := #27'[90m';
    cBrightRed: Result := #27'[91m';
    cBrightGreen: Result := #27'[92m';
    cBrightYellow: Result := #27'[93m';
    cBrightBlue: Result := #27'[94m';
    cBrightMagenta: Result := #27'[95m';
    cBrightCyan: Result := #27'[96m';
    cBrightWhite: Result := #27'[97m';
    else
      Result := '';
  end;
end;

function ColorText(const Text: string; Color: TColor): string;
begin
  if Color = cDefault then
    Result := Text
  else
    Result := GetColorCode(Color) + Text + ResetColor;
end;

function ResetColor: string;
begin
  Result := #27'[0m';
end;

// RGB Color utility functions
function RGB(R, G, B: Byte): TRGBColor;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function GetRGBColorCode(const Color: TRGBColor; Background: Boolean = False): string;
begin
  if Background then
    Result := Format(#27'[48;2;%d;%d;%dm', [Color.R, Color.G, Color.B])
  else
    Result := Format(#27'[38;2;%d;%d;%dm', [Color.R, Color.G, Color.B]);
end;

function ColorTextRGB(const Text: string; const Color: TRGBColor): string;
begin
  Result := GetRGBColorCode(Color) + Text + ResetColor;
end;

function ColorTextRGBBg(const Text: string; const FgColor, BgColor: TRGBColor): string;
begin
  Result := GetRGBColorCode(FgColor) + GetRGBColorCode(BgColor, True) + Text + ResetColor;
end;

// Gradient and animation functions
function HSVtoRGB(H, S, V: Double): TRGBColor;
var
  I: Integer;
  F, P, Q, T: Double;
begin
  // Ensure H is in range 0-360
  while H < 0 do H := H + 360;
  while H >= 360 do H := H - 360;
  
  // Ensure S and V are in range 0-1
  if S < 0 then S := 0;
  if S > 1 then S := 1;
  if V < 0 then V := 0;
  if V > 1 then V := 1;
  
  if S = 0 then
  begin
    // Achromatic (grey)
    Result.R := Round(V * 255);
    Result.G := Round(V * 255);
    Result.B := Round(V * 255);
  end
  else
  begin
    H := H / 60;
    I := Floor(H);
    F := H - I;
    P := V * (1 - S);
    Q := V * (1 - S * F);
    T := V * (1 - S * (1 - F));
    
    case I of
      0: begin Result.R := Round(V * 255); Result.G := Round(T * 255); Result.B := Round(P * 255); end;
      1: begin Result.R := Round(Q * 255); Result.G := Round(V * 255); Result.B := Round(P * 255); end;
      2: begin Result.R := Round(P * 255); Result.G := Round(V * 255); Result.B := Round(T * 255); end;
      3: begin Result.R := Round(P * 255); Result.G := Round(Q * 255); Result.B := Round(V * 255); end;
      4: begin Result.R := Round(T * 255); Result.G := Round(P * 255); Result.B := Round(V * 255); end;
      else begin Result.R := Round(V * 255); Result.G := Round(P * 255); Result.B := Round(Q * 255); end;
    end;
  end;
end;

function InterpolateRGB(const Color1, Color2: TRGBColor; t: Double): TRGBColor;
begin
  if t <= 0 then
    Result := Color1
  else if t >= 1 then
    Result := Color2
  else
  begin
    Result.R := Round(Color1.R + (Color2.R - Color1.R) * t);
    Result.G := Round(Color1.G + (Color2.G - Color1.G) * t);
    Result.B := Round(Color1.B + (Color2.B - Color1.B) * t);
  end;
end;

function GetRainbowColor(Position: Double): TRGBColor;
var
  Hue: Double;
begin
  // Ensure position is in range 0-1
  while Position < 0 do Position := Position + 1;
  while Position > 1 do Position := Position - 1;
  
  // Convert position to hue (0-360 degrees)
  Hue := Position * 360;
  
  // Create rainbow color with full saturation and value
  Result := HSVtoRGB(Hue, 1.0, 1.0);
end;

function CreateGradientLine(const Text: string; StartHue, EndHue: Double): string;
var
  I, Len: Integer;
  Position: Double;
  Color: TRGBColor;
  Char: string;
begin
  Result := AnsiString('');
  Len := Length(Text);
  
  if Len = 0 then
    Exit;
  
  for I := 1 to Len do
  begin
    if Len = 1 then
      Position := 0
    else
      Position := (I - 1) / (Len - 1);
    
    // Interpolate between start and end hue
    Color := HSVtoRGB(StartHue + (EndHue - StartHue) * Position, 1.0, 1.0);
    
    // Get single character
    Char := Text[I];
    
    // Add colored character
    Result := Result + ColorTextRGB(Char, Color);
  end;
end;

// Border style functions that ensure AnsiString types
function GetSingleBorder: TBorderChars;
begin
  Result.TopLeft := AnsiString('┌');
  Result.TopRight := AnsiString('┐');
  Result.BottomLeft := AnsiString('└');
  Result.BottomRight := AnsiString('┘');
  Result.Horizontal := AnsiString('─');
  Result.Vertical := AnsiString('│');
end;

function GetDoubleBorder: TBorderChars;
begin
  Result.TopLeft := AnsiString('╔');
  Result.TopRight := AnsiString('╗');
  Result.BottomLeft := AnsiString('╚');
  Result.BottomRight := AnsiString('╝');
  Result.Horizontal := AnsiString('═');
  Result.Vertical := AnsiString('║');
end;

function GetRoundedBorder: TBorderChars;
begin
  Result.TopLeft := AnsiString('╭');
  Result.TopRight := AnsiString('╮');
  Result.BottomLeft := AnsiString('╰');
  Result.BottomRight := AnsiString('╯');
  Result.Horizontal := AnsiString('─');
  Result.Vertical := AnsiString('│');
end;

function GetBoldBorder: TBorderChars;
begin
  Result.TopLeft := AnsiString('┏');
  Result.TopRight := AnsiString('┓');
  Result.BottomLeft := AnsiString('┗');
  Result.BottomRight := AnsiString('┛');
  Result.Horizontal := AnsiString('━');
  Result.Vertical := AnsiString('┃');
end;

constructor TStyle.Create;
begin
  inherited Create;
  FBorderStyle := bsNone;
  FBorderColor := cDefault;
  FBorderBackgroundColor := cDefault;
  FUseRGBBorderBackground := False;
  FBorderBackgroundColorRGB := RGB(0, 0, 0);
  FBorderRadius := 0;
  FTitle := AnsiString('');
  FTitlePosition := tpLeft;
  FTitleColor := cDefault;
  FWidth := 0;
  FHeight := 0;
  FContent := '';
  
  // Initialize background and content color properties
  FBackgroundColor := cDefault;
  FUseRGBBackground := False;
  FContentColor := cDefault;
  FUseRGBContent := False;
  // Initialize RGB colors to sensible defaults
  FBackgroundColorRGB := RGB(0, 0, 0);
  FContentColorRGB := RGB(255, 255, 255);
  
  // Initialize text wrapping properties
  FTextWrapping := twAuto;
  FWordWrapIndent := 0;
end;

procedure TStyle.SetBorderStyle(AStyle: TBorderStyle);
begin
  FBorderStyle := AStyle;
  FBorderChars := GetBorderChars;
end;

function TStyle.GetBorderChars: TBorderChars;
begin
  case FBorderStyle of
    bsSingle: Result := GetSingleBorder;
    bsDouble: Result := GetDoubleBorder;
    bsRounded: Result := GetRoundedBorder;
    bsBold: Result := GetBoldBorder;
    bsRadius:
      begin
        // For now, only radius 1 is supported, which is the same as bsRounded
        if FBorderRadius = 1 then
          Result := GetRoundedBorder
        else
          Result := GetSingleBorder; // Default to single for other radii
      end;
    else
      Result := GetSingleBorder; // Default to single border
  end;
end;

function SplitLines(const S: string): TStringArray;
var
  Start, P, LineCount, Len: Integer;
  Lines: TStringArray;
begin
  Start := 1;
  LineCount := 0;
  Len := Length(S);
  SetLength(Lines, 0);
  P := 1;
  while P <= Len do
  begin
    if (S[P] = #10) or (S[P] = #13) then
    begin
      if P > Start then
      begin
        SetLength(Lines, LineCount + 1);
        Lines[LineCount] := Copy(S, Start, P - Start);
        Inc(LineCount);
      end;
      // Handle CRLF
      if (S[P] = #13) and (P < Len) and (S[P+1] = #10) then
        Inc(P);
      Start := P + 1;
    end;
    Inc(P);
  end;
  if Start <= Len then
  begin
    SetLength(Lines, LineCount + 1);
    Lines[LineCount] := Copy(S, Start, Len - Start + 1);
  end;
  Result := Lines;
end;

function StripAnsiEscapes(const S: string): string;
var
  I: integer;
  InEscape: boolean;
begin
  Result := AnsiString('');
  I := 1;
  InEscape := false;
  while I <= Length(S) do
  begin
    if S[I] = #27 then // ESC character starts ANSI escape sequence
    begin
      InEscape := true;
    end
    else if InEscape and (S[I] = 'm') then // 'm' ends color escape sequence
    begin
      InEscape := false;
    end
    else if not InEscape then
    begin
      Result := Result + S[I];
    end;
    Inc(I);
  end;
end;

function Utf8CodepointCount(const S: string): integer;
begin
  // This is a simplified implementation that counts bytes, not codepoints.
  // It will work for ASCII but not for multi-byte UTF-8 characters.
  Result := Length(S);
end;

function Utf8DisplayWidth(const S: string): integer;
var
  CleanedString: string;
  I, Len, CharLen: integer;
begin
  Result := 0;
  CleanedString := StripAnsiEscapes(S);
  I := 1;
  Len := Length(CleanedString);
  while I <= Len do
  begin
    Result := Result + 1;
    CharLen := 1;
    if (Byte(CleanedString[I]) and $E0) = $C0 then
      CharLen := 2
    else if (Byte(CleanedString[I]) and $F0) = $E0 then
      CharLen := 3
    else if (Byte(CleanedString[I]) and $F8) = $F0 then
      CharLen := 4;
    // Note: This simplified version assumes all characters have a display width of 1.
    // A more advanced implementation would check for wide characters (e.g., CJK).
    I := I + CharLen;
  end;
end;

function TruncateAnsiStringAtDisplay(const S: string; MaxWidth: integer): string;
var
  I, CurrentWidth, CharLen: integer;
  InEscape: Boolean;
begin
  Result := AnsiString('');
  I := 1;
  CurrentWidth := 0;
  InEscape := False;
  
  while (I <= Length(S)) and (CurrentWidth < MaxWidth) do
  begin
    if S[I] = #27 then // Start of ANSI escape sequence
    begin
      InEscape := True;
      Result := Result + S[I];
      Inc(I);
    end
    else if InEscape then
    begin
      Result := Result + S[I];
      if S[I] = 'm' then // End of ANSI color escape sequence
        InEscape := False;
      Inc(I);
    end
    else
    begin
      // Regular character - check if it fits
      if CurrentWidth >= MaxWidth then
        Break;
        
      // Add character to result
      Result := Result + S[I];
      
      // Calculate character length for UTF-8
      CharLen := 1;
      if (Byte(S[I]) and $E0) = $C0 then
        CharLen := 2
      else if (Byte(S[I]) and $F0) = $E0 then
        CharLen := 3
      else if (Byte(S[I]) and $F8) = $F0 then
        CharLen := 4;
      
      // Add remaining bytes of UTF-8 character
      while (CharLen > 1) and (I + 1 <= Length(S)) do
      begin
        Inc(I);
        Result := Result + S[I];
        Dec(CharLen);
      end;
      
      Inc(CurrentWidth);
      Inc(I);
    end;
  end;
end;

function SplitAnsiStringAtDisplay(const S: string; SplitPos: integer; out BeforePart, AfterPart: string): Boolean;
var
  I, CurrentWidth, CharLen: integer;
  InEscape: Boolean;
begin
  Result := True;
  BeforePart := AnsiString('');
  AfterPart := AnsiString('');
  I := 1;
  CurrentWidth := 0;
  InEscape := False;
  
  // Build the before part
  while (I <= Length(S)) and (CurrentWidth < SplitPos) do
  begin
    if S[I] = #27 then // Start of ANSI escape sequence
    begin
      InEscape := True;
      BeforePart := BeforePart + S[I];
      Inc(I);
    end
    else if InEscape then
    begin
      BeforePart := BeforePart + S[I];
      if S[I] = 'm' then // End of ANSI color escape sequence
        InEscape := False;
      Inc(I);
    end
    else
    begin
      // Regular character
      BeforePart := BeforePart + S[I];
      
      // Calculate character length for UTF-8
      CharLen := 1;
      if (Byte(S[I]) and $E0) = $C0 then
        CharLen := 2
      else if (Byte(S[I]) and $F0) = $E0 then
        CharLen := 3
      else if (Byte(S[I]) and $F8) = $F0 then
        CharLen := 4;
      
      // Add remaining bytes of UTF-8 character
      while (CharLen > 1) and (I + 1 <= Length(S)) do
      begin
        Inc(I);
        BeforePart := BeforePart + S[I];
        Dec(CharLen);
      end;
      
      Inc(CurrentWidth);
      Inc(I);
    end;
  end;
  
  // The rest goes to AfterPart
  AfterPart := Copy(S, I, MaxInt);
end;

function JoinHorizontal(const Left, Right: string; Separator: string = ' '): string;
var
  LeftLines, RightLines: TStringArray;
  I, MaxLines: integer;
  LeftLine, RightLine: string;
  ResultLines: TStringArray;
begin
  // Split both strings into lines
  LeftLines := SplitLines(Left);
  RightLines := SplitLines(Right);
  
  // Find the maximum number of lines
  MaxLines := Length(LeftLines);
  if Length(RightLines) > MaxLines then
    MaxLines := Length(RightLines);
  
  // Build result line by line into array first
  SetLength(ResultLines, MaxLines);
  
  // Join line by line
  for I := 0 to MaxLines - 1 do
  begin
    // Get left line or empty string
    if I < Length(LeftLines) then
      LeftLine := LeftLines[I]
    else
      LeftLine := AnsiString('');
      
    // Get right line or empty string  
    if I < Length(RightLines) then
      RightLine := RightLines[I]
    else
      RightLine := AnsiString('');
    
    // Store combined line
    ResultLines[I] := LeftLine + Separator + RightLine;
  end;
  
  // Join all lines with line endings
  Result := AnsiString('');
  for I := 0 to High(ResultLines) do
  begin
    Result := Result + ResultLines[I];
    if I < High(ResultLines) then
      Result := Result + LineEnding;
  end;
end;

function JoinVertical(const Blocks: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Blocks) to High(Blocks) do
  begin
    Result := Result + Blocks[I];
    if I < High(Blocks) then
      Result := Result + LineEnding;
  end;
end;

function JoinVertical(const Blocks: TStringArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Blocks) to High(Blocks) do
  begin
    Result := Result + Blocks[I];
    if I < High(Blocks) then
      Result := Result + LineEnding;
  end;
end;

function TStyle.Render: string;
var
  Lines: TStringArray;
  I: integer;
  BorderChars: TBorderChars;
  PadLen: integer;
  LineWidth: integer;
  HorizontalLine: string;
  ContentWidth: integer;
  MaxContentLines: integer;
  TitleLen, TotalLen, StartPos, LeftLen, RightLen: integer;
  BackgroundColorCode, ContentColorCode: string;
  LineContent, PaddedLine: string;
  EmptyLine: string;
  ActualHeight: integer;
  LogFile: TextFile;
begin
  // Initialize debug logging
  AssignFile(LogFile, 'bobaui_debug.log');
  if FileExists('bobaui_debug.log') then
    Append(LogFile)
  else
    Rewrite(LogFile);
  
  WriteLn(LogFile, '=== TStyle.Render START ===');
  WriteLn(LogFile, 'FWidth: ', FWidth);
  WriteLn(LogFile, 'FHeight: ', FHeight);
  WriteLn(LogFile, 'FBorderStyle: ', Ord(FBorderStyle));
  WriteLn(LogFile, 'Content length: ', Length(FContent));
  // Handle no-border case with background support
  if FBorderStyle = bsNone then
  begin
    // Apply background and content colors to plain content
    if FUseRGBBackground or FUseRGBContent then
    begin
      // Apply text wrapping if needed and enabled
      if (FTextWrapping <> twNone) and (FWidth > 0) and NeedsWrapping(FContent, FWidth) then
        Lines := WrapTextAnsiAware(FContent, FWidth, FTextWrapping)
      else
        Lines := SplitLines(FContent);
      Result := AnsiString('');
      
      // Get color codes
      if FUseRGBBackground then
        BackgroundColorCode := GetRGBColorCode(FBackgroundColorRGB, True)
      else if FBackgroundColor <> cDefault then
        BackgroundColorCode := #27'[' + IntToStr(Ord(FBackgroundColor) + 40) + 'm'
      else
        BackgroundColorCode := '';
        
      if FUseRGBContent then
        ContentColorCode := GetRGBColorCode(FContentColorRGB, False)
      else if FContentColor <> cDefault then
        ContentColorCode := GetColorCode(FContentColor)
      else
        ContentColorCode := '';
      
      // Apply colors and padding to each line
      for I := 0 to High(Lines) do
      begin
        LineContent := BackgroundColorCode + ContentColorCode + Lines[I];
        
        // Pad line to width if specified
        if FWidth > 0 then
        begin
          PadLen := FWidth - Utf8DisplayWidth(Lines[I]);
          while PadLen > 0 do
          begin
            LineContent := LineContent + ' ';
            Dec(PadLen);
          end;
        end;
        
        LineContent := LineContent + ResetColor;
        Result := Result + LineContent;
        if I < High(Lines) then
          Result := Result + LineEnding;
      end;
      
      // Add empty lines if height is specified
      if FHeight > 0 then
      begin
        ActualHeight := Length(Lines);
        if FWidth > 0 then
        begin
          EmptyLine := BackgroundColorCode;
          for I := 1 to FWidth do
            EmptyLine := EmptyLine + ' ';
          EmptyLine := EmptyLine + ResetColor;
        end
        else
          EmptyLine := '';
          
        while ActualHeight < FHeight do
        begin
          Result := Result + LineEnding + EmptyLine;
          Inc(ActualHeight);
        end;
      end;
    end
    else
    begin
      // Apply text wrapping even without background colors
      if (FTextWrapping <> twNone) and (FWidth > 0) and NeedsWrapping(FContent, FWidth) then
      begin
        Lines := WrapTextAnsiAware(FContent, FWidth, FTextWrapping);
        Result := JoinVertical(Lines);
      end
      else
        Result := FContent;
    end;
    Exit;
  end;

  // Get border characters
  BorderChars := GetBorderChars;
  
  // Calculate width if not set (use display width)
  if FWidth = 0 then
  begin
    // First get the content lines to determine required width
    Lines := SplitLines(FContent);
    FWidth := 0;
    for I := 0 to High(Lines) do
    begin
      LineWidth := Utf8DisplayWidth(Lines[I]);
      if LineWidth > FWidth then
        FWidth := LineWidth;
    end;
    // Add border width
    FWidth := FWidth + GetFrameSize(FBorderStyle).Width;
  end;
  
  ContentWidth := GetContentWidth(FWidth, FBorderStyle);
  WriteLn(LogFile, 'ContentWidth calculated: ', ContentWidth);
  
  // Apply text wrapping if needed and enabled
  if (FTextWrapping <> twNone) and (ContentWidth > 0) and NeedsWrapping(FContent, ContentWidth) then
  begin
    WriteLn(LogFile, 'Text wrapping ENABLED - ContentWidth: ', ContentWidth);
    Lines := WrapTextAnsiAware(FContent, ContentWidth, FTextWrapping);
    WriteLn(LogFile, 'After wrapping - Lines count: ', Length(Lines));
  end
  else
  begin
    WriteLn(LogFile, 'Text wrapping DISABLED');
    Lines := SplitLines(FContent);
    WriteLn(LogFile, 'After splitting - Lines count: ', Length(Lines));
  end;
  
  // Get background and content color codes
  if FUseRGBBackground then
    BackgroundColorCode := GetRGBColorCode(FBackgroundColorRGB, True)
  else if FBackgroundColor <> cDefault then
    BackgroundColorCode := #27'[' + IntToStr(Ord(FBackgroundColor) + 40) + 'm'
  else
    BackgroundColorCode := '';
    
  if FUseRGBContent then
    ContentColorCode := GetRGBColorCode(FContentColorRGB, False)
  else if FContentColor <> cDefault then
    ContentColorCode := GetColorCode(FContentColor)
  else
    ContentColorCode := '';
  
  // Build the result
  Result := AnsiString('');
  
  // Build horizontal line using StringOfChar equivalent for UTF-8
  HorizontalLine := AnsiString('');
  for I := 1 to ContentWidth do
    HorizontalLine := HorizontalLine + BorderChars.Horizontal;
  
  // Top border (apply color if specified)
  if FBorderStyle <> bsNone then
  begin
    WriteLn(LogFile, 'BORDER: Rendering top border - ContentWidth: ', ContentWidth);
    WriteLn(LogFile, 'BORDER: TopLeft char: "', BorderChars.TopLeft, '"');
    Result := Result + ColorBorderText(BorderChars.TopLeft, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);

    if (FTitle <> '') and (Length(FTitle) + 4 <= ContentWidth) then
    begin
      TitleLen := Utf8DisplayWidth(FTitle);
      TotalLen := ContentWidth;

      case FTitlePosition of
        tpLeft: StartPos := 2;
        tpCenter: StartPos := (TotalLen - TitleLen) div 2;
        tpRight: StartPos := TotalLen - TitleLen - 1;
        else StartPos := 2;
      end;
      
      LeftLen := StartPos - 1;
      RightLen := TotalLen - LeftLen - TitleLen - 2;

      // Left part of border
      for I := 1 to LeftLen do
        Result := Result + ColorBorderText(BorderChars.Horizontal, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);

      // Title
      Result := Result + AnsiString(' ');
      Result := Result + ColorText(FTitle, FTitleColor);
      Result := Result + AnsiString(' ');

      // Right part of border
      for I := 1 to RightLen do
        Result := Result + ColorBorderText(BorderChars.Horizontal, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);
    end
    else
    begin
       Result := Result + ColorBorderText(HorizontalLine, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);
    end;

    WriteLn(LogFile, 'BORDER: TopRight char: "', BorderChars.TopRight, '"');
    Result := Result + ColorBorderText(BorderChars.TopRight, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground) + LineEnding;
    WriteLn(LogFile, 'BORDER: Top border complete - Total width: ', Utf8DisplayWidth(Result));
  end;
  
  // Content with side borders and background colors
  for I := 0 to High(Lines) do
  begin
    WriteLn(LogFile, 'BORDER: Line ', I, ' - Content: "', Lines[I], '"');
    WriteLn(LogFile, 'BORDER: Line ', I, ' - Content display width: ', Utf8DisplayWidth(Lines[I]));
    
    // Left border
    Result := Result + ColorBorderText(BorderChars.Vertical, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);
    
    // Content line with background and content colors
    PaddedLine := BackgroundColorCode + ContentColorCode + Lines[I];
    
    // Pad with background-colored spaces
    PadLen := ContentWidth - Utf8DisplayWidth(Lines[I]);
    WriteLn(LogFile, 'BORDER: Line ', I, ' - PadLen needed: ', PadLen);
    while PadLen > 0 do
    begin
      PaddedLine := PaddedLine + ' ';
      Dec(PadLen);
    end;
    
    // Close colors and add right border
    PaddedLine := PaddedLine + ResetColor;
    Result := Result + PaddedLine;
    
    WriteLn(LogFile, 'BORDER: Line ', I, ' - Adding right border: "', BorderChars.Vertical, '"');
    Result := Result + ColorBorderText(BorderChars.Vertical, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground) + LineEnding;
    WriteLn(LogFile, 'BORDER: Line ', I, ' - Complete line width: ', Utf8DisplayWidth(Result));
  end;
  
  // Add empty lines if height is specified and we need more lines
  if FHeight > 0 then
  begin
    // Calculate how many more lines we need (subtract 2 for top and bottom borders)
    PadLen := FHeight - 2 - Length(Lines);
    while PadLen > 0 do
    begin
      Result := Result + ColorBorderText(BorderChars.Vertical, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);
      
      // Add empty line with background color
      EmptyLine := BackgroundColorCode;
      for I := 1 to ContentWidth do
        EmptyLine := EmptyLine + ' ';
      EmptyLine := EmptyLine + ResetColor;
      Result := Result + EmptyLine;
      
      Result := Result + ColorBorderText(BorderChars.Vertical, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground) + LineEnding;
      Dec(PadLen);
    end;
  end;
  
  // Bottom border - rebuild horizontal line to avoid corruption
  HorizontalLine := AnsiString('');
  for I := 1 to ContentWidth do
    HorizontalLine := HorizontalLine + BorderChars.Horizontal;
  WriteLn(LogFile, 'BORDER: Rendering bottom border - ContentWidth: ', ContentWidth);
  WriteLn(LogFile, 'BORDER: BottomLeft: "', BorderChars.BottomLeft, '", BottomRight: "', BorderChars.BottomRight, '"');
  Result := Result + ColorBorderText(BorderChars.BottomLeft + HorizontalLine + BorderChars.BottomRight, FBorderColor, FBorderBackgroundColor, FBorderBackgroundColorRGB, FUseRGBBorderBackground);
  
  WriteLn(LogFile, '=== TStyle.Render COMPLETE ===');
  WriteLn(LogFile, 'Final result lines count: ', Length(SplitLines(Result)));
  WriteLn(LogFile, 'Final result total width: ', Utf8DisplayWidth(Result));
  WriteLn(LogFile, '');
  CloseFile(LogFile);
end;

// Background styling convenience functions

function CreateBackgroundStyle(Width, Height: Integer; BgColor: TRGBColor): TStyle;
begin
  Result := TStyle.Create;
  Result.Width := Width;
  Result.Height := Height;
  Result.BackgroundColorRGB := BgColor;
  Result.UseRGBBackground := True;
  Result.BorderStyle := bsNone;
end;

function CreateColoredStyle(Width, Height: Integer; FgColor, BgColor: TRGBColor): TStyle;
begin
  Result := TStyle.Create;
  Result.Width := Width;
  Result.Height := Height;
  Result.ContentColorRGB := FgColor;
  Result.BackgroundColorRGB := BgColor;
  Result.UseRGBContent := True;
  Result.UseRGBBackground := True;
  Result.BorderStyle := bsNone;
end;

function PadWithBackground(const Content: string; Width, Height: Integer; BgColor: TRGBColor): string;
var
  Style: TStyle;
begin
  Style := CreateBackgroundStyle(Width, Height, BgColor);
  try
    Style.Content := Content;
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function FillArea(Width, Height: Integer; BgColor: TRGBColor): string;
var
  Style: TStyle;
begin
  Style := CreateBackgroundStyle(Width, Height, BgColor);
  try
    Style.Content := '';
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function ApplyBackgroundToText(const Text: string; BgColor: TRGBColor): string;
var
  Lines: TStringArray;
  I: Integer;
  BackgroundCode: string;
begin
  BackgroundCode := GetRGBColorCode(BgColor, True);
  Lines := SplitLines(Text);
  Result := '';
  
  for I := 0 to High(Lines) do
  begin
    Result := Result + BackgroundCode + Lines[I] + ResetColor;
    if I < High(Lines) then
      Result := Result + LineEnding;
  end;
end;

function ColorBorderText(const Text: string; FgColor: TColor; BgColor: TColor; BgColorRGB: TRGBColor; UseRGBBg: Boolean): string;
var
  FgColorCode, BgColorCode: string;
begin
  // Get foreground color code
  if FgColor = cDefault then
    FgColorCode := ''
  else
    FgColorCode := GetColorCode(FgColor);
  
  // Get background color code
  if UseRGBBg then
    BgColorCode := GetRGBColorCode(BgColorRGB, True)
  else if BgColor <> cDefault then
    BgColorCode := #27'[' + IntToStr(Ord(BgColor) + 40) + 'm'
  else
    BgColorCode := '';
  
  // Apply colors
  if (FgColorCode <> '') or (BgColorCode <> '') then
    Result := FgColorCode + BgColorCode + Text + ResetColor
  else
    Result := Text;
end;

// Text wrapping delegation to bobaflow.pas

function IsWordBreakCharacter(C: Char): Boolean;
begin
  Result := bobaflow.IsWordBreakCharacter(C);
end;

function NeedsWrapping(const Text: string; MaxWidth: Integer): Boolean;
begin
  Result := bobaflow.NeedsWrapping(Text, MaxWidth);
end;

function TrimTrailingSpaces(const S: string): string;
begin
  Result := bobaflow.TrimTrailingSpaces(S);
end;

function CountLeadingSpaces(const S: string): Integer;
begin
  Result := bobaflow.CountLeadingSpaces(S);
end;

function PreserveIndentation(const OriginalLine, WrappedLine: string): string;
begin
  Result := bobaflow.PreserveIndentation(OriginalLine, WrappedLine);
end;

function ExtractAnsiState(const Text: string): TAnsiState;
begin
  Result := bobaflow.ExtractAnsiState(Text);
end;

function ApplyAnsiState(const Text: string; const State: TAnsiState): string;
begin
  Result := bobaflow.ApplyAnsiState(Text, State);
end;

function CarryForwardAnsiState(const PreviousLine, NextLine: string): string;
begin
  Result := bobaflow.CarryForwardAnsiState(PreviousLine, NextLine);
end;

function FindWrapPosition(const Line: string; MaxWidth: Integer; Mode: TTextWrapping): Integer;
begin
  Result := bobaflow.FindWrapPosition(Line, MaxWidth, Mode);
end;

function WrapTextHard(const Text: string; MaxWidth: Integer): TStringArray;
begin
  Result := bobaflow.WrapTextHard(Text, MaxWidth);
end;

function WrapText(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
begin
  Result := bobaflow.WrapText(Text, MaxWidth, Mode);
end;

function WrapTextAnsiAware(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
begin
  Result := bobaflow.WrapTextAnsiAware(Text, MaxWidth, Mode);
end;

function SplitTextPreservingAnsi(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
begin
  Result := bobaflow.SplitTextPreservingAnsi(Text, MaxWidth, Mode);
end;

function GetFrameSize(BorderStyle: TBorderStyle; HasTitle: Boolean = False): TFrameSize;
begin
  Result.Width := 0;
  Result.Height := 0;
  
  case BorderStyle of
    bsNone:
      begin
        Result.Width := 0;
        Result.Height := 0;
      end;
    bsSingle, bsDouble, bsRounded, bsBold, bsRadius:
      begin
        Result.Width := 2;  // Left + right borders
        Result.Height := 2; // Top + bottom borders
      end;
  end;
  
  // Title doesn't add to frame size as it's part of the top border
end;

function GetContentWidth(TotalWidth: Integer; BorderStyle: TBorderStyle): Integer;
var
  FrameSize: TFrameSize;
begin
  FrameSize := GetFrameSize(BorderStyle);
  Result := TotalWidth - FrameSize.Width;
  if Result < 0 then Result := 0;
end;

function GetContentHeight(TotalHeight: Integer; BorderStyle: TBorderStyle; HasTitle: Boolean = False): Integer;
var
  FrameSize: TFrameSize;
begin
  FrameSize := GetFrameSize(BorderStyle, HasTitle);
  Result := TotalHeight - FrameSize.Height;
  if Result < 0 then Result := 0;
end;

end. 
