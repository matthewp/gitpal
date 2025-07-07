unit claude_oauth_provider;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils, models;

type
  { TClaudeOAuthProvider - OAuth-enabled Claude provider }
  TClaudeOAuthProvider = class(TBaseToolProvider, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FAccessToken: string;
    FBaseUrl: string;
    FSystemMessage: string;
    
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
    
    // Tool calling helper methods
    function SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
    function SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
    function ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
    function ChatCompletionWithToolsInternal(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
  protected
    // Override for base tool provider
    function ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
      const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; override;
  public
    constructor Create(const AAccessToken: string; const ASystemMessage: string = 'You are Claude, an AI assistant created by Anthropic.');
    destructor Destroy; override;
    
    // ILLMProvider implementation
    function ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    procedure ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
      ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
    function GetProviderName: string;
    function GetDefaultModel: string;
    function ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    function SupportsToolCalling: Boolean;
    
    // OAuth-specific properties
    property SystemMessage: string read FSystemMessage write FSystemMessage;
  end;

implementation

constructor TClaudeOAuthProvider.Create(const AAccessToken: string; const ASystemMessage: string);
begin
  inherited Create;
  FAccessToken := AAccessToken;
  FBaseUrl := 'https://api.anthropic.com/v1';
  FSystemMessage := ASystemMessage;
  
  FHttpClient := TFPHttpClient.Create(nil);
  FHttpClient.AddHeader('content-type', 'application/json');
  FHttpClient.AddHeader('authorization', 'Bearer ' + FAccessToken);
  FHttpClient.AddHeader('anthropic-version', '2023-06-01');
  FHttpClient.AddHeader('anthropic-beta', 'oauth-2025-04-20');
end;

destructor TClaudeOAuthProvider.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TClaudeOAuthProvider.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := 'system';
    lmrUser: Result := 'user';
    lmrAssistant: Result := 'assistant';
    lmrTool: Result := 'tool';
  else
    Result := 'user';
  end;
end;

function TClaudeOAuthProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LUsageObj: TJSONObject;
  LContentArray: TJSONArray;
  TextContent: string;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  try
    LJsonData := GetJSON(AJsonString);
  except
    on E: Exception do
      raise Exception.Create('Failed to parse JSON response: ' + E.Message);
  end;

  try
    if not (LJsonData is TJSONObject) then
      raise Exception.Create('Invalid JSON response');

    LJsonObject := TJSONObject(LJsonData);
    
    // Check for error response
    if LJsonObject.Find('error') <> nil then
    begin
      if LJsonObject.Objects['error'] <> nil then
        raise Exception.Create('API error: ' + LJsonObject.Objects['error'].Get('message', 'Unknown error'))
      else
        raise Exception.Create('Unknown API error');
    end;
    
    // Parse model and ID
    if LJsonObject.Find('model') <> nil then
      Result.Model := LJsonObject.Strings['model'];
    if LJsonObject.Find('id') <> nil then
      Result.ID := LJsonObject.Strings['id'];

    // Parse content
    TextContent := '';
    if LJsonObject.Find('content') <> nil then
    begin
      LContentArray := LJsonObject.Arrays['content'];
      for I := 0 to LContentArray.Count - 1 do
      begin
        if LContentArray.Objects[I].Find('text') <> nil then
        begin
          if TextContent <> '' then
            TextContent := TextContent + #10;
          TextContent := TextContent + LContentArray.Objects[I].Strings['text'];
        end;
      end;
    end;
    
    // Create response structure
    SetLength(Result.Choices, 1);
    Result.Choices[0].Index := 0;
    Result.Choices[0].Message.Role := lmrAssistant;
    Result.Choices[0].Message.Content := TextContent;
    
    // Parse tool calls from content array
    if LJsonObject.Find('content') <> nil then
    begin
      LContentArray := LJsonObject.Arrays['content'];
      Result.Choices[0].Message.ToolCalls := ParseToolUseFromContent(LContentArray);
    end
    else
    begin
      SetLength(Result.Choices[0].Message.ToolCalls, 0);
    end;
    
    if LJsonObject.Find('stop_reason') <> nil then
      Result.Choices[0].FinishReason := LJsonObject.Strings['stop_reason'];

    // Parse usage information
    LUsageObj := LJsonObject.Objects['usage'];
    if LUsageObj <> nil then
    begin
      if LUsageObj.Find('input_tokens') <> nil then
        Result.Usage.PromptTokens := LUsageObj.Integers['input_tokens'];
      if LUsageObj.Find('output_tokens') <> nil then
        Result.Usage.CompletionTokens := LUsageObj.Integers['output_tokens'];
      Result.Usage.TotalTokens := Result.Usage.PromptTokens + Result.Usage.CompletionTokens;
    end;

  finally
    LJsonData.Free;
  end;
end;

function TClaudeOAuthProvider.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('max_tokens', AMaxTokens);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // For OAuth, use the configured system message
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our configured system message
        Continue;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', LMessage.Content);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include the configured system message for OAuth
    LRequestJson.Add('system', FSystemMessage);
    LRequestJson.Add('messages', LMessagesArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      LResponseText := FHttpClient.Post(FBaseUrl + '/messages');
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

procedure TClaudeOAuthProvider.ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LDeltaObj: TJSONObject;
  LContent: string;
begin
  if AEventType = 'content_block_delta' then
  begin
    try
      LJsonData := GetJSON(AData);
      if LJsonData is TJSONObject then
      begin
        LJsonObject := TJSONObject(LJsonData);
        LDeltaObj := LJsonObject.Objects['delta'];
        if LDeltaObj <> nil then
        begin
          if LDeltaObj.Find('text') <> nil then
          begin
            LContent := LDeltaObj.Strings['text'];
            ACallback(LContent, False);
          end;
        end;
      end;
    except
      // Ignore parsing errors
    end;
    
    try
      LJsonData.Free;
    except
      // Ignore cleanup errors
    end;
  end
  else if AEventType = 'message_stop' then
  begin
    ACallback('', True);
  end;
end;

procedure TClaudeOAuthProvider.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
  ACallback: TLLMStreamCallback; ATemperature: Double; AMaxTokens: Integer);
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseStream: TStringStream;
  LResponseLines: TStringList;
  LStreamClient: TFPHttpClient;
  LIndex: Integer;
  LLine: string;
  LEventType: string;
  LEventData: string;
  LResponseCode: Integer;
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('max_tokens', AMaxTokens);
    LRequestJson.Add('stream', True);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // For OAuth, use the configured system message
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our configured system message
        Continue;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', LMessage.Content);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include the configured system message for OAuth
    LRequestJson.Add('system', FSystemMessage);
    LRequestJson.Add('messages', LMessagesArray);

    // Create fresh client for streaming request
    LStreamClient := TFPHttpClient.Create(nil);
    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    LResponseStream := TStringStream.Create('');
    try
      LStreamClient.AddHeader('content-type', 'application/json');
      LStreamClient.AddHeader('authorization', 'Bearer ' + FAccessToken);
      LStreamClient.AddHeader('anthropic-version', '2023-06-01');
      LStreamClient.AddHeader('anthropic-beta', 'oauth-2025-04-20');
      
      LStreamClient.RequestBody := LRequestBody;
      
      try
        LStreamClient.Post(FBaseUrl + '/messages', LResponseStream);
        LResponseCode := LStreamClient.ResponseStatusCode;
        
        if LResponseCode <> 200 then
        begin
          ACallback('Error: HTTP ' + IntToStr(LResponseCode) + ' - ' + LResponseStream.DataString, True);
          Exit;
        end;
      except
        on E: Exception do
        begin
          ACallback('Error: ' + E.Message, True);
          Exit;
        end;
      end;
      
      // Parse the response line by line for SSE events
      LResponseLines := TStringList.Create;
      try
        LResponseLines.Text := LResponseStream.DataString;
        LEventType := '';
        LEventData := '';
        
        for LIndex := 0 to LResponseLines.Count - 1 do
        begin
          LLine := Trim(LResponseLines.Strings[LIndex]);
          
          if LLine = '' then
          begin
            // Empty line marks end of event
            if (LEventType <> '') and (LEventData <> '') then
            begin
              ParseStreamEvent(LEventType, LEventData, ACallback);
              LEventType := '';
              LEventData := '';
            end;
          end
          else if Copy(LLine, 1, 6) = 'event:' then
          begin
            LEventType := Trim(Copy(LLine, 7, Length(LLine)));
          end
          else if Copy(LLine, 1, 5) = 'data:' then
          begin
            LEventData := Trim(Copy(LLine, 6, Length(LLine)));
          end;
        end;
        
        // Handle any remaining event
        if (LEventType <> '') and (LEventData <> '') then
          ParseStreamEvent(LEventType, LEventData, ACallback);
          
      finally
        LResponseLines.Free;
      end;
    finally
      LStreamClient.Free;
      LRequestBody.Free;
      LResponseStream.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TClaudeOAuthProvider.GetProviderName: string;
begin
  Result := 'Claude-OAuth';
end;

function TClaudeOAuthProvider.GetDefaultModel: string;
begin
  Result := 'claude-3-5-sonnet-latest';
end;

function TClaudeOAuthProvider.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  CurrentMessages: array of TLLMMessage;
  Response: TLLMChatCompletionResponse;
  ToolCall: TToolCall;
  ToolResult: TToolResult;
  i, LIndex: Integer;
  MaxIterations: Integer;
begin
  // Initialize conversation with original messages
  SetLength(CurrentMessages, Length(AMessages));
  for i := 0 to High(AMessages) do
    CurrentMessages[i] := AMessages[i];
  
  MaxIterations := 10; // Prevent infinite loops
  
  for i := 1 to MaxIterations do
  begin
    // Make API request with tools
    Response := ChatCompletionWithToolsInternal(AModel, CurrentMessages, ATools, ATemperature, AMaxTokens);
    
    // Check if AI wants to use tools
    if Length(Response.Choices) > 0 then
    begin
      if Length(Response.Choices[0].Message.ToolCalls) > 0 then
      begin
        // Add assistant's message with tool calls to conversation FIRST
        SetLength(CurrentMessages, Length(CurrentMessages) + 1);
        CurrentMessages[High(CurrentMessages)] := Response.Choices[0].Message;
        
        // Execute each tool call and add results
        for LIndex := 0 to Length(Response.Choices[0].Message.ToolCalls) - 1 do
        begin
          ToolCall := Response.Choices[0].Message.ToolCalls[LIndex];
          
          try
            ToolResult := AToolContext.ExecuteTool(ToolCall);
            ToolResult.FunctionName := ToolCall.FunctionName;
          except
            on E: Exception do
            begin
              ToolResult.ToolCallId := ToolCall.Id;
              ToolResult.Content := 'Tool execution failed: ' + E.Message;
              ToolResult.IsError := True;
              ToolResult.FunctionName := ToolCall.FunctionName;
            end;
          end;
          
          // Add tool result to conversation
          SetLength(CurrentMessages, Length(CurrentMessages) + 1);
          CurrentMessages[High(CurrentMessages)].Role := lmrTool;
          CurrentMessages[High(CurrentMessages)].Content := ToolResult.Content;
          CurrentMessages[High(CurrentMessages)].Name := '';
          CurrentMessages[High(CurrentMessages)].ToolCallId := ToolResult.ToolCallId;
          CurrentMessages[High(CurrentMessages)].FunctionName := ToolResult.FunctionName;
          SetLength(CurrentMessages[High(CurrentMessages)].ToolCalls, 0);
        end;
      end
      else
      begin
        // AI provided final response, we're done
        Break;
      end;
    end
    else
    begin
      // No response choices, break
      Break;
    end;
  end;
  
  Result := Response;
end;

function TClaudeOAuthProvider.SupportsToolCalling: Boolean;
begin
  Result := True;
end;

function TClaudeOAuthProvider.ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
  const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
begin
  Result := ChatCompletionWithToolsInternal(AModel, AMessages, ATools, ATemperature, AMaxTokens);
end;

function TClaudeOAuthProvider.ChatCompletionWithToolsInternal(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LToolsArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
  LContentArray: TJSONArray;
  LContentObj: TJSONObject;
  i: Integer;
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('max_tokens', AMaxTokens);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // Add tools if provided
    if Length(ATools) > 0 then
    begin
      LToolsArray := SerializeClaudeTools(ATools);
      LRequestJson.Add('tools', LToolsArray);
    end;

    // For OAuth, use the configured system message
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our configured system message
        Continue;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        
        // Claude uses content blocks
        LContentArray := TJSONArray.Create;
        
        // Check if this is a tool result message
        if LMessage.Role = lmrTool then
        begin
          // Tool result message
          LContentObj := TJSONObject.Create;
          LContentObj.Add('type', 'tool_result');
          LContentObj.Add('tool_use_id', LMessage.ToolCallId);
          LContentObj.Add('content', LMessage.Content);
          LContentArray.Add(LContentObj);
        end
        else if (LMessage.Role = lmrAssistant) and (Length(LMessage.ToolCalls) > 0) then
        begin
          // Assistant message with tool calls
          if LMessage.Content <> '' then
          begin
            LContentObj := TJSONObject.Create;
            LContentObj.Add('type', 'text');
            LContentObj.Add('text', LMessage.Content);
            LContentArray.Add(LContentObj);
          end;
          
          // Add tool_use blocks
          for i := 0 to Length(LMessage.ToolCalls) - 1 do
          begin
            LContentObj := TJSONObject.Create;
            LContentObj.Add('type', 'tool_use');
            LContentObj.Add('id', LMessage.ToolCalls[i].Id);
            LContentObj.Add('name', LMessage.ToolCalls[i].FunctionName);
            // Parse arguments JSON and add as object
            try
              LContentObj.Add('input', GetJSON(LMessage.ToolCalls[i].Arguments));
            except
              // If JSON parsing fails, add empty object
              LContentObj.Add('input', TJSONObject.Create);
            end;
            LContentArray.Add(LContentObj);
          end;
        end
        else
        begin
          // Regular text message
          LContentObj := TJSONObject.Create;
          LContentObj.Add('type', 'text');
          LContentObj.Add('text', LMessage.Content);
          LContentArray.Add(LContentObj);
        end;
        
        LMessageObj.Add('content', LContentArray);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include the configured system message for OAuth
    LRequestJson.Add('system', FSystemMessage);
    LRequestJson.Add('messages', LMessagesArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      LResponseText := FHttpClient.Post(FBaseUrl + '/messages');
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TClaudeOAuthProvider.SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
var
  i: Integer;
  ToolObj: TJSONObject;
  InputSchemaObj: TJSONObject;
  PropertiesObj: TJSONObject;
  RequiredArray: TJSONArray;
  j: Integer;
begin
  Result := TJSONArray.Create;
  
  for i := 0 to High(ATools) do
  begin
    ToolObj := TJSONObject.Create;
    ToolObj.Add('name', ATools[i].Name);
    ToolObj.Add('description', ATools[i].Description);
    
    // Create input schema
    InputSchemaObj := TJSONObject.Create;
    InputSchemaObj.Add('type', 'object');
    
    // Create properties object
    PropertiesObj := TJSONObject.Create;
    RequiredArray := TJSONArray.Create;
    
    for j := 0 to High(ATools[i].Parameters) do
    begin
      PropertiesObj.Add(ATools[i].Parameters[j].Name, SerializeClaudeToolParameter(ATools[i].Parameters[j]));
      if ATools[i].Parameters[j].Required then
        RequiredArray.Add(ATools[i].Parameters[j].Name);
    end;
    
    InputSchemaObj.Add('properties', PropertiesObj);
    if RequiredArray.Count > 0 then
      InputSchemaObj.Add('required', RequiredArray)
    else
      RequiredArray.Free;
    
    ToolObj.Add('input_schema', InputSchemaObj);
    Result.Add(ToolObj);
  end;
end;

function TClaudeOAuthProvider.SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
begin
  Result := TJSONObject.Create;
  
  case AParam.ParamType of
    tptString:
      Result.Add('type', 'string');
    tptInteger:
      Result.Add('type', 'integer');
    tptNumber:
      Result.Add('type', 'number');
    tptBoolean:
      Result.Add('type', 'boolean');
    tptArray:
      Result.Add('type', 'array');
    tptObject:
      Result.Add('type', 'object');
  end;
  
  if AParam.Description <> '' then
    Result.Add('description', AParam.Description);
end;

function TClaudeOAuthProvider.ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
var
  i: Integer;
  ContentItem: TJSONObject;
  ToolUseCount: Integer;
  ToolIndex: Integer;
begin
  // Initialize result to empty array
  Result := nil;
  SetLength(Result, 0);
  
  // First pass: count tool_use blocks
  ToolUseCount := 0;
  for i := 0 to AContentArray.Count - 1 do
  begin
    if AContentArray.Items[i] is TJSONObject then
    begin
      ContentItem := TJSONObject(AContentArray.Items[i]);
      if ContentItem.Strings['type'] = 'tool_use' then
        Inc(ToolUseCount);
    end;
  end;
  
  if ToolUseCount = 0 then Exit;
  
  SetLength(Result, ToolUseCount);
  ToolIndex := 0;
  
  // Second pass: extract tool calls
  for i := 0 to AContentArray.Count - 1 do
  begin
    if AContentArray.Items[i] is TJSONObject then
    begin
      ContentItem := TJSONObject(AContentArray.Items[i]);
      if ContentItem.Strings['type'] = 'tool_use' then
      begin
        Result[ToolIndex].Id := ContentItem.Strings['id'];
        Result[ToolIndex].FunctionName := ContentItem.Strings['name'];
        // Convert input object back to JSON string
        if ContentItem.Find('input') <> nil then
          Result[ToolIndex].Arguments := ContentItem.Objects['input'].AsJSON
        else
          Result[ToolIndex].Arguments := '{}';
        Inc(ToolIndex);
      end;
    end;
  end;
end;

end.