{$codepage UTF8}
unit bobaflow;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Classes;

type
  // Array of strings for splitting and wrapping
  TStringArray = array of string;

  // Text wrapping modes
  TTextWrapping = (
    twNone,   // No wrapping (current behavior)
    twWord,   // Wrap at word boundaries (preferred)
    twChar,   // Wrap at any character (fallback)
    twAuto    // Automatic - try word, fall back to char if needed
  );

  // Frame size record for layout calculations
  TFrameSize = record
    Width, Height: Integer;
  end;

  // ANSI state tracking for color preservation
  TAnsiState = record
    ForegroundColor: string;
    BackgroundColor: string;
    HasForeground: Boolean;
    HasBackground: Boolean;
  end;

// Core text wrapping functions
function WrapText(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
function WrapTextAnsiAware(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
function WrapTextHard(const Text: string; MaxWidth: Integer): TStringArray;

// Helper functions
function NeedsWrapping(const Text: string; MaxWidth: Integer): Boolean;
function IsWordBreakCharacter(C: Char): Boolean;
function FindWrapPosition(const Line: string; MaxWidth: Integer; Mode: TTextWrapping): Integer;

// ANSI sequence handling
function ExtractAnsiState(const Text: string): TAnsiState;
function ApplyAnsiState(const Text: string; const State: TAnsiState): string;
function CarryForwardAnsiState(const PreviousLine, NextLine: string): string;
function SplitTextPreservingAnsi(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;

// Basic string utilities  
function SplitLines(const S: string): TStringArray;
function StripAnsiEscapes(const S: string): string;
function Utf8DisplayWidth(const S: string): integer;

// Utility functions for better text handling
function TrimTrailingSpaces(const S: string): string;
function CountLeadingSpaces(const S: string): Integer;
function PreserveIndentation(const OriginalLine, WrappedLine: string): string;

implementation

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

function IsWordBreakCharacter(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9) or (C = '-') or (C = #10) or (C = #13);
end;

function NeedsWrapping(const Text: string; MaxWidth: Integer): Boolean;
var
  Lines: TStringArray;
  I: Integer;
begin
  Result := False;
  if MaxWidth <= 0 then Exit;
  
  Lines := SplitLines(Text);
  for I := 0 to High(Lines) do
  begin
    if Utf8DisplayWidth(Lines[I]) > MaxWidth then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TrimTrailingSpaces(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] = ' ') do
    Dec(I);
  if I < Length(Result) then
    Result := Copy(Result, 1, I);
end;

function CountLeadingSpaces(const S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if S[I] = ' ' then
      Inc(Result)
    else
      Break;
  end;
end;

function PreserveIndentation(const OriginalLine, WrappedLine: string): string;
var
  IndentCount: Integer;
  I: Integer;
begin
  IndentCount := CountLeadingSpaces(OriginalLine);
  Result := WrappedLine;
  
  // Add indentation to wrapped line
  for I := 1 to IndentCount do
    Result := ' ' + Result;
end;

function ExtractAnsiState(const Text: string): TAnsiState;
var
  I: Integer;
  EscapeSeq: string;
  InEscape: Boolean;
begin
  Result.ForegroundColor := '';
  Result.BackgroundColor := '';
  Result.HasForeground := False;
  Result.HasBackground := False;
  
  I := 1;
  InEscape := False;
  
  while I <= Length(Text) do
  begin
    if (I <= Length(Text) - 1) and (Text[I] = #27) and (Text[I + 1] = '[') then
    begin
      InEscape := True;
      EscapeSeq := #27'[';
      I := I + 2;
      
      // Read until 'm'
      while (I <= Length(Text)) and (Text[I] <> 'm') do
      begin
        EscapeSeq := EscapeSeq + Text[I];
        Inc(I);
      end;
      
      if (I <= Length(Text)) and (Text[I] = 'm') then
      begin
        EscapeSeq := EscapeSeq + 'm';
        
        // Check if this is a foreground color (30-37, 90-97, 38;...)
        if (Pos('[3', EscapeSeq) > 0) or (Pos('[9', EscapeSeq) > 0) or (Pos('[38;', EscapeSeq) > 0) then
        begin
          Result.ForegroundColor := EscapeSeq;
          Result.HasForeground := True;
        end
        // Check if this is a background color (40-47, 100-107, 48;...)
        else if (Pos('[4', EscapeSeq) > 0) or (Pos('[10', EscapeSeq) > 0) or (Pos('[48;', EscapeSeq) > 0) then
        begin
          Result.BackgroundColor := EscapeSeq;
          Result.HasBackground := True;
        end
        // Check for reset
        else if (EscapeSeq = #27'[0m') or (EscapeSeq = #27'[m') then
        begin
          Result.ForegroundColor := '';
          Result.BackgroundColor := '';
          Result.HasForeground := False;
          Result.HasBackground := False;
        end;
      end;
      Inc(I);
    end
    else
      Inc(I);
  end;
end;

function ApplyAnsiState(const Text: string; const State: TAnsiState): string;
begin
  Result := Text;
  if State.HasForeground and (State.ForegroundColor <> '') then
    Result := State.ForegroundColor + Result;
  if State.HasBackground and (State.BackgroundColor <> '') then
    Result := State.BackgroundColor + Result;
end;

function CarryForwardAnsiState(const PreviousLine, NextLine: string): string;
var
  State: TAnsiState;
begin
  State := ExtractAnsiState(PreviousLine);
  Result := ApplyAnsiState(NextLine, State);
end;

function FindWrapPosition(const Line: string; MaxWidth: Integer; Mode: TTextWrapping): Integer;
var
  DisplayPos, BytePos: Integer;
  LastWordBreak: Integer;
  CleanLine: string;
  InEscape: Boolean;
  I: Integer;
begin
  
  Result := Length(Line);
  if MaxWidth <= 0 then
    Exit;
  
  CleanLine := StripAnsiEscapes(Line);
  
  if Length(CleanLine) <= MaxWidth then
    Exit;
  
  case Mode of
    twNone:
      begin
        Result := Length(Line); // No wrapping
      end;
      
    twChar:
      begin
        // Character-level wrapping - find byte position for display position
        DisplayPos := 0;
        BytePos := 1;
        InEscape := False;
        
        while (BytePos <= Length(Line)) and (DisplayPos < MaxWidth) do
        begin
          if (BytePos <= Length(Line) - 1) and (Line[BytePos] = #27) and (Line[BytePos + 1] = '[') then
          begin
            InEscape := True;
            Inc(BytePos, 2);
          end
          else if InEscape then
          begin
            if Line[BytePos] = 'm' then
              InEscape := False;
            Inc(BytePos);
          end
          else
          begin
            Inc(DisplayPos);
            Inc(BytePos);
          end;
        end;
        
        Result := BytePos - 1;
      end;
      
    twWord, twAuto:
      begin
        // Word-level wrapping - find last word break before MaxWidth
        DisplayPos := 0;
        BytePos := 1;
        LastWordBreak := 0;
        InEscape := False;
        
        while (BytePos <= Length(Line)) and (DisplayPos <= MaxWidth) do
        begin
          if (BytePos <= Length(Line) - 1) and (Line[BytePos] = #27) and (Line[BytePos + 1] = '[') then
          begin
            InEscape := True;
            Inc(BytePos, 2);
          end
          else if InEscape then
          begin
            if Line[BytePos] = 'm' then
              InEscape := False;
            Inc(BytePos);
          end
          else
          begin
            if IsWordBreakCharacter(Line[BytePos]) then
              LastWordBreak := BytePos;
            
            Inc(DisplayPos);
            if DisplayPos > MaxWidth then
              Break;
            Inc(BytePos);
          end;
        end;
        
        if LastWordBreak > 0 then
          Result := LastWordBreak
        else if Mode = twAuto then
        begin
          Result := FindWrapPosition(Line, MaxWidth, twChar); // Fall back to character wrapping
          Exit;
        end
        else
          Result := BytePos - 1;
      end;
  end;
  
end;

function WrapTextHard(const Text: string; MaxWidth: Integer): TStringArray;
var
  Lines: TStringArray;
  I, J: Integer;
  CurrentLine: string;
  WrapPos: Integer;
begin
  SetLength(Result, 0);
  if MaxWidth <= 0 then Exit;
  
  Lines := SplitLines(Text);
  
  for I := 0 to High(Lines) do
  begin
    CurrentLine := Lines[I];
    
    while Utf8DisplayWidth(StripAnsiEscapes(CurrentLine)) > MaxWidth do
    begin
      WrapPos := FindWrapPosition(CurrentLine, MaxWidth, twChar);
      
      if WrapPos > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TrimTrailingSpaces(Copy(CurrentLine, 1, WrapPos));
        CurrentLine := Copy(CurrentLine, WrapPos + 1, MaxInt);
        
        // Carry forward ANSI state
        if High(Result) >= 0 then
          CurrentLine := CarryForwardAnsiState(Result[High(Result)], CurrentLine);
      end
      else
        Break;
    end;
    
    // Add remaining part
    if Length(CurrentLine) > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CurrentLine;
    end;
  end;
end;

function WrapText(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
var
  Lines: TStringArray;
  I: Integer;
  CurrentLine: string;
  WrapPos: Integer;
  IndentCount: Integer;
begin
  SetLength(Result, 0);
  if MaxWidth <= 0 then Exit;
  
  Lines := SplitLines(Text);
  
  for I := 0 to High(Lines) do
  begin
    CurrentLine := Lines[I];
    IndentCount := CountLeadingSpaces(CurrentLine);
    
    while Utf8DisplayWidth(StripAnsiEscapes(CurrentLine)) > MaxWidth do
    begin
      WrapPos := FindWrapPosition(CurrentLine, MaxWidth, Mode);
      
      if WrapPos > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TrimTrailingSpaces(Copy(CurrentLine, 1, WrapPos));
        CurrentLine := Copy(CurrentLine, WrapPos + 1, MaxInt);
        
        // Preserve indentation for continuation lines
        if IndentCount > 0 then
          CurrentLine := PreserveIndentation(Lines[I], CurrentLine);
      end
      else
        Break;
    end;
    
    // Add remaining part
    if Length(CurrentLine) > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CurrentLine;
    end;
  end;
end;

function WrapTextAnsiAware(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
begin
  Result := SplitTextPreservingAnsi(Text, MaxWidth, Mode);
end;

function SplitTextPreservingAnsi(const Text: string; MaxWidth: Integer; Mode: TTextWrapping = twWord): TStringArray;
var
  Lines: TStringArray;
  I: Integer;
  CurrentLine: string;
  WrapPos: Integer;
  PreviousState: TAnsiState;
  IndentCount: Integer;
  CleanCurrentLine: string;
begin
  
  SetLength(Result, 0);
  if MaxWidth <= 0 then
    Exit;
  
  Lines := SplitLines(Text);
  
  for I := 0 to High(Lines) do
  begin
    CurrentLine := Lines[I];
    CleanCurrentLine := StripAnsiEscapes(CurrentLine);
    IndentCount := CountLeadingSpaces(CleanCurrentLine);
    
    while Utf8DisplayWidth(StripAnsiEscapes(CurrentLine)) > MaxWidth do
    begin
      WrapPos := FindWrapPosition(CurrentLine, MaxWidth, Mode);
      
      if WrapPos > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TrimTrailingSpaces(Copy(CurrentLine, 1, WrapPos));
        
        // Extract ANSI state from the wrapped line
        PreviousState := ExtractAnsiState(Result[High(Result)]);
        
        CurrentLine := Copy(CurrentLine, WrapPos + 1, MaxInt);
        
        // Apply preserved ANSI state to continuation line
        CurrentLine := ApplyAnsiState(CurrentLine, PreviousState);
        
        // Preserve indentation for continuation lines
        if IndentCount > 0 then
          CurrentLine := PreserveIndentation(Lines[I], CurrentLine);
      end
      else
        Break;
    end;
    
    // Add remaining part
    if Length(CurrentLine) > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CurrentLine;
    end;
  end;
end;


end.