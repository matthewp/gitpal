unit logging;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

// Simple debug logging procedure
procedure DebugLog(const Message: string);

implementation

uses
  SysUtils;

var
  DebugEnabled: Boolean = False;
  DebugInitialized: Boolean = False;

procedure InitializeDebugLogging;
var
  DebugEnvVar: string;
begin
  if DebugInitialized then Exit;
  
  DebugEnvVar := GetEnvironmentVariable('GITPAL_DEBUG');
  DebugEnabled := (DebugEnvVar <> '');
  DebugInitialized := True;
  
  if DebugEnabled then
    DebugLog('Debug logging enabled - writing to gitpal-debug.log');
end;

procedure DebugLog(const Message: string);
var
  LogFile: TextFile;
  Timestamp: string;
begin
  // Initialize on first call
  if not DebugInitialized then
    InitializeDebugLogging;
    
  // Fast exit if debug is disabled
  if not DebugEnabled then Exit;
  
  try
    // Create timestamp
    Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    
    // Append to log file
    AssignFile(LogFile, 'gitpal-debug.log');
    if FileExists('gitpal-debug.log') then
      Append(LogFile)
    else
      Rewrite(LogFile);
    
    try
      WriteLn(LogFile, Timestamp + ' ' + Message);
    finally
      CloseFile(LogFile);
    end;
  except
    // Silently ignore logging errors - don't break the main functionality
  end;
end;

end.