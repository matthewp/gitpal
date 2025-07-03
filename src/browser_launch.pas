{$mode objfpc}
{$codepage UTF8}
{$H+}

unit browser_launch;

interface

uses
  SysUtils, Process;

type
  TBrowserLaunchResult = record
    Success: Boolean;
    ErrorMessage: AnsiString;
  end;

function LaunchBrowser(const URL: AnsiString): TBrowserLaunchResult;
procedure DisplayURLFallback(const URL: AnsiString);

implementation

{$IFDEF LINUX}
function TryLinuxFallbackBrowsers(const URL: AnsiString; var Result: TBrowserLaunchResult): Boolean; forward;
{$ENDIF}

function LaunchBrowser(const URL: AnsiString): TBrowserLaunchResult;
var
  Process: TProcess;
  Args: array of AnsiString;
  I: Integer;
begin
  Result.Success := False;
  Result.ErrorMessage := AnsiString('');
  
  Process := TProcess.Create(nil);
  try
    Process.Options := [poWaitOnExit, poUsePipes];
    
    {$IFDEF DARWIN}
    // macOS: Use 'open' command
    Process.Executable := 'open';
    SetLength(Args, 1);
    Args[0] := URL;
    {$ENDIF}
    {$IFDEF LINUX}
    // Linux: Try xdg-open first, then fallback browsers
    Process.Executable := 'xdg-open';
    SetLength(Args, 1);
    Args[0] := URL;
    {$ENDIF}
    {$IFDEF WINDOWS}
    // Windows: Use 'start' command
    Process.Executable := 'cmd';
    SetLength(Args, 3);
    Args[0] := '/c';
    Args[1] := 'start';
    Args[2] := URL;
    {$ENDIF}
    {$IF not defined(DARWIN) and not defined(LINUX) and not defined(WINDOWS)}
    // Unknown platform
    Result.ErrorMessage := AnsiString('Unsupported platform for browser launch');
    Exit;
    {$ENDIF}
    
    // Set process parameters
    for I := 0 to High(Args) do
      Process.Parameters.Add(string(Args[I]));
    
    try
      Process.Execute;
      Process.WaitOnExit;
      
      if Process.ExitStatus = 0 then
      begin
        Result.Success := True;
      end
      else
      begin
        Result.ErrorMessage := AnsiString('Browser command failed with exit code: ' + IntToStr(Process.ExitStatus));
        
        {$IFDEF LINUX}
        // On Linux, try fallback browsers if xdg-open failed
        if not TryLinuxFallbackBrowsers(URL, Result) then
          Result.Success := False;
        {$ENDIF}
      end;
    except
      on E: Exception do
      begin
        Result.ErrorMessage := AnsiString('Failed to launch browser: ' + E.Message);
        
        {$IFDEF LINUX}
        // On Linux, try fallback browsers if xdg-open failed
        if not TryLinuxFallbackBrowsers(URL, Result) then
          Result.Success := False;
        {$ENDIF}
      end;
    end;
    
  finally
    Process.Free;
  end;
end;

{$IFDEF LINUX}
function TryLinuxFallbackBrowsers(const URL: AnsiString; var Result: TBrowserLaunchResult): Boolean;
var
  Browsers: array[0..4] of AnsiString;
  Process: TProcess;
  I: Integer;
begin
  Result.Success := False;
  
  // Common Linux browsers to try
  Browsers[0] := 'firefox';
  Browsers[1] := 'google-chrome';
  Browsers[2] := 'chromium';
  Browsers[3] := 'chromium-browser';
  Browsers[4] := 'sensible-browser';
  
  for I := 0 to High(Browsers) do
  begin
    Process := TProcess.Create(nil);
    try
      Process.Executable := string(Browsers[I]);
      Process.Parameters.Add(string(URL));
      Process.Options := [poWaitOnExit, poUsePipes];
      
      try
        Process.Execute;
        Process.WaitOnExit;
        
        if Process.ExitStatus = 0 then
        begin
          Result.Success := True;
          Result.ErrorMessage := AnsiString('');
          Exit(True);
        end;
      except
        // Continue to next browser
      end;
      
    finally
      Process.Free;
    end;
  end;
  
  Result.ErrorMessage := AnsiString('No working browser found. Tried: xdg-open, firefox, google-chrome, chromium, chromium-browser, sensible-browser');
  Exit(False);
end;
{$ENDIF}

procedure DisplayURLFallback(const URL: AnsiString);
begin
  WriteLn('');
  WriteLn('Unable to automatically open browser.');
  WriteLn('Please manually navigate to the following URL to complete authentication:');
  WriteLn('');
  WriteLn(string(URL));
  WriteLn('');
  WriteLn('After completing authentication, return to this terminal.');
end;

end.