unit models;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils;

type
  { TLLMMessageRole }
  TLLMMessageRole = (lmrSystem, lmrUser, lmrAssistant, lmrTool);

  { TLLMStreamCallback - Callback for receiving streaming chunks }
  TLLMStreamCallback = procedure(const AChunk: string; AFinished: Boolean) of object;

  { Tool calling parameter types }
  TToolParameterType = (tptString, tptInteger, tptNumber, tptBoolean, tptArray, tptObject);
  
  { Tool parameter definition }
  TToolParameter = record
    Name: string;
    ParamType: TToolParameterType;
    Description: string;
    Required: Boolean;
    EnumValues: array of string; // For restricted string values
  end;
  
  { Tool function definition }
  TToolFunction = record
    Name: string;
    Description: string;
    Parameters: array of TToolParameter;
  end;
  
  { Tool call from AI }
  TToolCall = record
    Id: string;          // Unique identifier for this call
    FunctionName: string;
    Arguments: string;   // JSON string of arguments
  end;
  
  { Tool call result }
  TToolResult = record
    ToolCallId: string;  // Matches TToolCall.Id
    Content: string;     // Result content or error message
    IsError: Boolean;    // True if tool execution failed
    FunctionName: string; // Name of the function that was called
  end;

  { Enhanced message with tool support }
  TLLMMessage = record
    Role: TLLMMessageRole;
    Content: string;
    Name: string;
    ToolCallId: string;
    FunctionName: string; // Name of function for tool result messages
    ToolCalls: array of TToolCall; // For assistant messages with tool calls
  end;

  { TLLMChatCompletionChoice }
  TLLMChatCompletionChoice = record
    Index: Integer;
    Message: TLLMMessage;
    FinishReason: string;
  end;

  { TLLMUsage }
  TLLMUsage = record
    PromptTokens: Integer;
    CompletionTokens: Integer;
    TotalTokens: Integer;
  end;

  { TLLMChatCompletionResponse }
  TLLMChatCompletionResponse = record
    ID: string;
    ObjectStr: string;
    Created: Int64;
    Model: string;
    Choices: array of TLLMChatCompletionChoice;
    Usage: TLLMUsage;
  end;

  { Array types (needed for interfaces) }
  TToolFunctionArray = array of TToolFunction;
  TToolCallArray = array of TToolCall;
  TLLMMessageArray = array of TLLMMessage;

  { Tool execution context interface }
  IToolContext = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function ExecuteTool(const ToolCall: TToolCall): TToolResult;
    function GetAvailableTools: TToolFunctionArray;
  end;

  { Callback for executing tools }
  TToolExecuteCallback = function(const ToolCall: TToolCall): TToolResult of object;

  { Abstract base class for tool context implementations }
  TBaseToolContext = class(TInterfacedObject, IToolContext)
  protected
    function CreateToolResult(const AToolCallId: string; const AContent: string; AIsError: Boolean = False): TToolResult;
    function SafeParseToolArguments(const JsonString: string; out JsonObj: TJSONObject): Boolean;
  public
    function ExecuteTool(const ToolCall: TToolCall): TToolResult; virtual; abstract;
    function GetAvailableTools: TToolFunctionArray; virtual; abstract;
  end;

  { Exception for tool execution errors }
  EToolExecutionError = class(Exception)
  private
    FToolCallId: string;
    FToolName: string;
  public
    constructor Create(const AToolCallId, AToolName, AMsg: string);
    property ToolCallId: string read FToolCallId;
    property ToolName: string read FToolName;
  end;

  { Tool call tracking for loop prevention }
  TToolCallTracker = class
  private
    FCallCount: Integer;
    FMaxCalls: Integer;
    FCallHistory: array of string; // Track function names
  public
    constructor Create(AMaxCalls: Integer = 10);
    function CanMakeCall(const FunctionName: string): Boolean;
    procedure RecordCall(const FunctionName: string);
    procedure Reset;
    property CallCount: Integer read FCallCount;
  end;

  { Base tool-enabled provider with common conversation loop }
  TBaseToolProvider = class(TInterfacedObject)
  protected
    function HasToolCalls(const Response: TLLMChatCompletionResponse): Boolean;
    procedure AddAssistantMessage(var Messages: TLLMMessageArray; const Response: TLLMChatCompletionResponse);
    procedure AddToolResultMessage(var Messages: TLLMMessageArray; const ToolResult: TToolResult);
    function ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
      const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; virtual; abstract;
  public
    function ChatCompletionWithToolsBase(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
  end;

  { ILLMProvider - Common interface for all LLM providers }
  ILLMProvider = interface
    ['{B8E5F4A2-1D2C-4E3F-8A9B-5C6D7E8F9A0B}']
    // Existing methods
    function ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    procedure ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
      ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
    function GetProviderName: string;
    function GetDefaultModel: string;
    
    // New tool calling methods
    function ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    function SupportsToolCalling: Boolean;
  end;




implementation




{ TBaseToolContext }

function TBaseToolContext.CreateToolResult(const AToolCallId: string; const AContent: string; AIsError: Boolean): TToolResult;
begin
  Result.ToolCallId := AToolCallId;
  Result.Content := AContent;
  Result.IsError := AIsError;
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

{ EToolExecutionError }

constructor EToolExecutionError.Create(const AToolCallId, AToolName, AMsg: string);
begin
  inherited Create(AMsg);
  FToolCallId := AToolCallId;
  FToolName := AToolName;
end;

{ TToolCallTracker }

constructor TToolCallTracker.Create(AMaxCalls: Integer);
begin
  inherited Create;
  FMaxCalls := AMaxCalls;
  FCallCount := 0;
  SetLength(FCallHistory, 0);
end;

function TToolCallTracker.CanMakeCall(const FunctionName: string): Boolean;
begin
  Result := FCallCount < FMaxCalls;
end;

procedure TToolCallTracker.RecordCall(const FunctionName: string);
begin
  Inc(FCallCount);
  SetLength(FCallHistory, Length(FCallHistory) + 1);
  FCallHistory[High(FCallHistory)] := FunctionName;
end;

procedure TToolCallTracker.Reset;
begin
  FCallCount := 0;
  SetLength(FCallHistory, 0);
end;

{ TBaseToolProvider }

function TBaseToolProvider.HasToolCalls(const Response: TLLMChatCompletionResponse): Boolean;
begin
  Result := (Length(Response.Choices) > 0) and 
            (Length(Response.Choices[0].Message.ToolCalls) > 0);
end;

procedure TBaseToolProvider.AddAssistantMessage(var Messages: TLLMMessageArray; const Response: TLLMChatCompletionResponse);
var
  NewLength: Integer;
  i: Integer;
begin
  if Length(Response.Choices) = 0 then Exit;
  
  NewLength := Length(Messages) + 1;
  SetLength(Messages, NewLength);
  
  // Careful copying of message with dynamic arrays
  Messages[High(Messages)].Role := Response.Choices[0].Message.Role;
  Messages[High(Messages)].Content := Response.Choices[0].Message.Content;
  Messages[High(Messages)].Name := Response.Choices[0].Message.Name;
  Messages[High(Messages)].ToolCallId := Response.Choices[0].Message.ToolCallId;
  SetLength(Messages[High(Messages)].ToolCalls, Length(Response.Choices[0].Message.ToolCalls));
  for i := 0 to High(Response.Choices[0].Message.ToolCalls) do
    Messages[High(Messages)].ToolCalls[i] := Response.Choices[0].Message.ToolCalls[i];
end;

procedure TBaseToolProvider.AddToolResultMessage(var Messages: TLLMMessageArray; const ToolResult: TToolResult);
var
  NewLength: Integer;
  ToolMessage: TLLMMessage;
begin
  
  NewLength := Length(Messages) + 1;
  SetLength(Messages, NewLength);
  
  // According to Claude documentation, tool results should be user messages
  // with special content structure, not separate "tool" role messages
  ToolMessage.Role := lmrUser;
  ToolMessage.Content := ToolResult.Content; // This will be handled specially in Claude serialization
  ToolMessage.Name := AnsiString('');
  ToolMessage.ToolCallId := ToolResult.ToolCallId; // Store tool call ID for Claude serialization
  ToolMessage.FunctionName := ToolResult.FunctionName; // Preserve function name for response serialization
  SetLength(ToolMessage.ToolCalls, 0);
  
  Messages[High(Messages)] := ToolMessage;
end;

function TBaseToolProvider.ChatCompletionWithToolsBase(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  CurrentMessages: TLLMMessageArray;
  ToolsArray: TToolFunctionArray;
  Response: TLLMChatCompletionResponse;
  ToolCall: TToolCall;
  ToolResult: TToolResult;
  Tracker: TToolCallTracker;
  i, j: Integer;
begin
  
  // Copy tools to typed array
  SetLength(ToolsArray, Length(ATools));
  for i := 0 to High(ATools) do
    ToolsArray[i] := ATools[i];
  
  // Initialize conversation with original messages (careful copying)
  SetLength(CurrentMessages, Length(AMessages));
  
  for i := 0 to High(AMessages) do
  begin
    CurrentMessages[i].Role := AMessages[i].Role;
    CurrentMessages[i].Content := AMessages[i].Content;
    CurrentMessages[i].Name := AMessages[i].Name;
    CurrentMessages[i].ToolCallId := AMessages[i].ToolCallId;
    SetLength(CurrentMessages[i].ToolCalls, Length(AMessages[i].ToolCalls));
    for j := 0 to High(AMessages[i].ToolCalls) do
      CurrentMessages[i].ToolCalls[j] := AMessages[i].ToolCalls[j];
  end;
  
  Tracker := TToolCallTracker.Create(10); // Max 10 tool call iterations
  try
    repeat
      // Send request with current messages and tool definitions
      Response := ChatCompletionInternal(AModel, CurrentMessages, ToolsArray, ATemperature, AMaxTokens);
      
      // Check if AI wants to use tools
      if HasToolCalls(Response) then
      begin
        // Add assistant's tool call message to conversation
        AddAssistantMessage(CurrentMessages, Response);
        
        // Execute each tool call
        for i := 0 to Length(Response.Choices[0].Message.ToolCalls) - 1 do
        begin
          ToolCall := Response.Choices[0].Message.ToolCalls[i];
          
          // Check loop prevention
          if not Tracker.CanMakeCall(ToolCall.FunctionName) then
          begin
            raise EToolExecutionError.Create(ToolCall.Id, ToolCall.FunctionName, 
              'Maximum tool call iterations exceeded (' + IntToStr(Tracker.CallCount) + ')');
          end;
          
          Tracker.RecordCall(ToolCall.FunctionName);
          
          try
            ToolResult := AToolContext.ExecuteTool(ToolCall);
          except
            on E: Exception do
            begin
              // Convert any exception to tool error result
              ToolResult.ToolCallId := ToolCall.Id;
              ToolResult.Content := 'Tool execution failed: ' + E.Message;
              ToolResult.IsError := True;
            end;
          end;
          
          // Add tool result to conversation
          AddToolResultMessage(CurrentMessages, ToolResult);
        end;
        
        // Continue conversation with tool results
        // (This will loop back to send another request)
      end
      else
      begin
        // AI provided final response, we're done
        Break;
      end;
    until False;
    
    Result := Response;
  finally
    Tracker.Free;
  end;
end;

end.