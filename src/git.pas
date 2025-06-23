unit git;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  SysUtils, Process, Classes;

type
  { Git operation result with comprehensive error information }
  TGitResult = record
    Success: Boolean;
    Output: string;
    ErrorMessage: string;
    ExitCode: Integer;
  end;

  { Shared git repository operations with security and consistency }
  TGitRepository = class
  private
    class function SanitizeGitArgument(const Arg: string): string;
    class function ValidateGitReference(const Ref: string): Boolean;
    class function ExecuteRaw(const Args: array of string; const WorkingDir: string = ''): TGitResult;
  public
    // Core repository detection
    class function IsRepository(const Path: string = '.'): Boolean;
    
    // Low-level git command execution
    class function Execute(const Args: array of string): TGitResult; overload;
    class function Execute(const Args: array of string; const WorkingDir: string): TGitResult; overload;
    
    // High-level operations for common git tasks
    class function GetDiff(const Options: array of string): TGitResult;
    class function Commit(const Message: string): TGitResult;
    class function Add(const Files: array of string): TGitResult;
    class function GetTags(const SortOptions: string = ''): TGitResult;
    class function GetCommitRange(const FromRef, ToRef: string): TGitResult;
    
    // Utility functions
    class function HasUnstagedChanges: TGitResult;
    class function GetStagedDiff: TGitResult;
  end;

implementation

{ TGitRepository }

class function TGitRepository.SanitizeGitArgument(const Arg: string): string;
begin
  Result := Arg;
  
  // Remove dangerous characters that could be used for command injection
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '', [rfReplaceAll]);
  Result := StringReplace(Result, '$', '', [rfReplaceAll]);
  Result := StringReplace(Result, '`', '', [rfReplaceAll]);
  Result := StringReplace(Result, #0, '', [rfReplaceAll]);
  
  // Limit length to prevent buffer overflow
  if Length(Result) > 255 then
    Result := Copy(Result, 1, 255);
end;

class function TGitRepository.ValidateGitReference(const Ref: string): Boolean;
begin
  Result := (Length(Ref) > 0) and 
            (Pos('..', Ref) = 0) and     // No path traversal
            (Ref[1] <> '-') and          // No option injection
            (Pos(#0, Ref) = 0) and       // No null injection
            (Length(Ref) <= 200);        // Reasonable length limit
end;

class function TGitRepository.ExecuteRaw(const Args: array of string; const WorkingDir: string): TGitResult;
var
  GitProcess: TProcess;
  SanitizedArgs: array of string;
  I: Integer;
  OutputStr: string;
  Buffer: array[0..2047] of char;
  BytesRead: Integer;
  OriginalDir: string;
  NeedRestoreDir: Boolean;
begin
  // Initialize result
  Result.Success := False;
  Result.Output := '';
  Result.ErrorMessage := '';
  Result.ExitCode := -1;
  
  // Sanitize all arguments
  SetLength(SanitizedArgs, Length(Args));
  for I := 0 to High(Args) do
    SanitizedArgs[I] := SanitizeGitArgument(Args[I]);
  
  // Handle working directory change if specified
  NeedRestoreDir := (WorkingDir <> '') and (WorkingDir <> '.');
  if NeedRestoreDir then
  begin
    OriginalDir := GetCurrentDir;
    if not SetCurrentDir(WorkingDir) then
    begin
      Result.ErrorMessage := 'Cannot change to directory: ' + WorkingDir;
      Exit;
    end;
  end;
  
  GitProcess := TProcess.Create(nil);
  try
    GitProcess.Executable := 'git';
    
    // Add sanitized arguments
    for I := 0 to High(SanitizedArgs) do
      GitProcess.Parameters.Add(SanitizedArgs[I]);
    
    GitProcess.Options := [poUsePipes, poWaitOnExit, poStderrToOutPut];
    
    try
      GitProcess.Execute;
      
      // Read all output
      OutputStr := '';
      while GitProcess.Running do
      begin
        if GitProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            OutputStr := OutputStr + Copy(string(Buffer), 1, BytesRead);
        end;
      end;
      
      // Read any remaining output after process ends
      while GitProcess.Output.NumBytesAvailable > 0 do
      begin
        BytesRead := GitProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          OutputStr := OutputStr + Copy(string(Buffer), 1, BytesRead);
      end;
      
      Result.ExitCode := GitProcess.ExitStatus;
      Result.Success := GitProcess.ExitStatus = 0;
      
      if Result.Success then
        Result.Output := Trim(OutputStr)
      else
        Result.ErrorMessage := Trim(OutputStr);
        
    except
      on E: Exception do
      begin
        Result.ErrorMessage := 'Git execution error: ' + E.Message;
        Result.ExitCode := -1;
      end;
    end;
  finally
    GitProcess.Free;
    
    // Restore original directory if we changed it
    if NeedRestoreDir then
      SetCurrentDir(OriginalDir);
  end;
end;

class function TGitRepository.IsRepository(const Path: string): Boolean;
var
  GitResult: TGitResult;
begin
  GitResult := ExecuteRaw(['rev-parse', '--git-dir'], Path);
  Result := GitResult.Success;
end;

class function TGitRepository.Execute(const Args: array of string): TGitResult;
begin
  Result := ExecuteRaw(Args, '');
end;

class function TGitRepository.Execute(const Args: array of string; const WorkingDir: string): TGitResult;
begin
  Result := ExecuteRaw(Args, WorkingDir);
end;

class function TGitRepository.GetDiff(const Options: array of string): TGitResult;
var
  Args: array of string;
  I: Integer;
begin
  SetLength(Args, Length(Options) + 1);
  Args[0] := 'diff';
  for I := 0 to High(Options) do
    Args[I + 1] := Options[I];
  
  Result := Execute(Args);
end;

class function TGitRepository.Commit(const Message: string): TGitResult;
begin
  if Trim(Message) = '' then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Commit message cannot be empty';
    Result.ExitCode := -1;
    Exit;
  end;
  
  Result := Execute(['commit', '-m', Message]);
end;

class function TGitRepository.Add(const Files: array of string): TGitResult;
var
  Args: array of string;
  I: Integer;
begin
  if Length(Files) = 0 then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'No files specified to add';
    Result.ExitCode := -1;
    Exit;
  end;
  
  SetLength(Args, Length(Files) + 1);
  Args[0] := 'add';
  for I := 0 to High(Files) do
    Args[I + 1] := Files[I];
  
  Result := Execute(Args);
end;

class function TGitRepository.GetTags(const SortOptions: string): TGitResult;
var
  Args: array of string;
begin
  if SortOptions <> '' then
  begin
    SetLength(Args, 3);
    Args[0] := 'tag';
    Args[1] := '-l';
    Args[2] := SortOptions;
  end
  else
  begin
    SetLength(Args, 2);
    Args[0] := 'tag';
    Args[1] := '-l';
  end;
  
  Result := Execute(Args);
end;

class function TGitRepository.GetCommitRange(const FromRef, ToRef: string): TGitResult;
begin
  if not ValidateGitReference(FromRef) or not ValidateGitReference(ToRef) then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Invalid git reference provided';
    Result.ExitCode := -1;
    Exit;
  end;
  
  Result := Execute(['log', '--oneline', FromRef + '..' + ToRef]);
end;

class function TGitRepository.HasUnstagedChanges: TGitResult;
var
  DiffResult: TGitResult;
begin
  DiffResult := GetDiff(['--name-only']);
  
  Result.Success := DiffResult.Success;
  Result.ErrorMessage := DiffResult.ErrorMessage;
  Result.ExitCode := DiffResult.ExitCode;
  
  if Result.Success then
  begin
    // If there's output, there are unstaged changes
    Result.Output := BoolToStr(Trim(DiffResult.Output) <> '', True);
  end
  else
  begin
    Result.Output := 'false';
  end;
end;

class function TGitRepository.GetStagedDiff: TGitResult;
begin
  Result := GetDiff(['--cached']);
end;

end.