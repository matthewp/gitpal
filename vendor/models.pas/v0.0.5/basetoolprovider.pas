unit basetoolprovider;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, models, toolcontext;

type
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
  TBaseToolProvider = class
  protected
    function HasToolCalls(const Response: TLLMChatCompletionResponse): Boolean;
    procedure AddAssistantMessage(var Messages: array of TLLMMessage; const Response: TLLMChatCompletionResponse);
    procedure AddToolResultMessage(var Messages: array of TLLMMessage; const ToolResult: TToolResult);
    function ChatCompletionInternal(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; virtual; abstract;
  public
    function ChatCompletionWithToolsBase(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
  end;

implementation

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

procedure TBaseToolProvider.AddAssistantMessage(var Messages: array of TLLMMessage; const Response: TLLMChatCompletionResponse);
var
  NewLength: Integer;
begin
  if Length(Response.Choices) = 0 then Exit;
  
  NewLength := Length(Messages) + 1;
  SetLength(Messages, NewLength);
  Messages[High(Messages)] := Response.Choices[0].Message;
end;

procedure TBaseToolProvider.AddToolResultMessage(var Messages: array of TLLMMessage; const ToolResult: TToolResult);
var
  NewLength: Integer;
  ToolMessage: TLLMMessage;
begin
  NewLength := Length(Messages) + 1;
  SetLength(Messages, NewLength);
  
  ToolMessage.Role := lmrTool;
  ToolMessage.Content := ToolResult.Content;
  ToolMessage.Name := AnsiString('');
  ToolMessage.ToolCallId := ToolResult.ToolCallId;
  ToolMessage.FunctionName := ToolResult.FunctionName;  // Copy function name for Gemini API
  SetLength(ToolMessage.ToolCalls, 0);
  
  Messages[High(Messages)] := ToolMessage;
end;

function TBaseToolProvider.ChatCompletionWithToolsBase(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  CurrentMessages: array of TLLMMessage;
  Response: TLLMChatCompletionResponse;
  ToolCall: TToolCall;
  ToolResult: TToolResult;
  Tracker: TToolCallTracker;
  i: Integer;
begin
  // Initialize conversation with original messages
  SetLength(CurrentMessages, Length(AMessages));
  for i := 0 to High(AMessages) do
    CurrentMessages[i] := AMessages[i];
  
  Tracker := TToolCallTracker.Create(10); // Max 10 tool call iterations
  try
    repeat
      // Send request with current messages and tool definitions
      Response := ChatCompletionInternal(AModel, CurrentMessages, ATools, ATemperature, AMaxTokens);
      
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
            // Ensure FunctionName is set for API compatibility (especially Gemini)
            ToolResult.FunctionName := ToolCall.FunctionName;
          except
            on E: Exception do
            begin
              // Convert any exception to tool error result
              ToolResult.ToolCallId := ToolCall.Id;
              ToolResult.Content := 'Tool execution failed: ' + E.Message;
              ToolResult.IsError := True;
              ToolResult.FunctionName := ToolCall.FunctionName;
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