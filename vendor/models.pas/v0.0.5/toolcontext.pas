unit toolcontext;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, models;

type
  { Callback for executing tools }
  TToolExecuteCallback = function(const ToolCall: TToolCall): TToolResult of object;
  
  { Tool execution context interface }
  IToolContext = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function ExecuteTool(const ToolCall: TToolCall): TToolResult;
    function GetAvailableTools: array of TToolFunction;
  end;

  { Abstract base class for tool context implementations }
  TBaseToolContext = class(TInterfacedObject, IToolContext)
  protected
    function CreateToolResult(const AToolCallId: string; const AContent: string; AIsError: Boolean = False): TToolResult;
    function SafeParseToolArguments(const JsonString: string; out JsonObj: TJSONObject): Boolean;
  public
    function ExecuteTool(const ToolCall: TToolCall): TToolResult; virtual; abstract;
    function GetAvailableTools: array of TToolFunction; virtual; abstract;
  end;

implementation

uses
  fpjson, jsonparser;

{ TBaseToolContext }

function TBaseToolContext.CreateToolResult(const AToolCallId: string; const AContent: string; AIsError: Boolean): TToolResult;
begin
  Result.ToolCallId := AToolCallId;
  Result.Content := AContent;
  Result.IsError := AIsError;
  Result.FunctionName := AnsiString(''); // Will be set by provider based on ToolCall.FunctionName
end;

function TBaseToolContext.SafeParseToolArguments(const JsonString: string; out JsonObj: TJSONObject): Boolean;
var
  JsonData: TJSONData;
begin
  Result := False;
  JsonObj := nil;
  try
    JsonData := GetJSON(JsonString);
    if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);
      Result := True;
    end else
      JsonData.Free;
  except
    on E: Exception do
    begin
      if Assigned(JsonData) then JsonData.Free;
      // Log error but don't re-raise
    end;
  end;
end;

end.