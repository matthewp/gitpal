{$mode objfpc}
{$codepage UTF8}
{$H+}

unit claude_oauth_adapter;

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils, models, logging;

type
  { TClaudeOAuthAdapter - Adapter for Claude provider that uses OAuth Bearer tokens }
  TClaudeOAuthAdapter = class(TInterfacedObject, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FAccessToken: AnsiString;
    FBaseUrl: AnsiString;
    
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
    
    // Tool calling helper methods
    function SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
    function SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
    function ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
    function ChatCompletionWithToolsInternal(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
  public
    constructor Create(const AAccessToken: AnsiString);
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
  end;

implementation

constructor TClaudeOAuthAdapter.Create(const AAccessToken: AnsiString);
begin
  inherited Create;
  FAccessToken := AAccessToken;
  FBaseUrl := AnsiString('https://api.anthropic.com/v1');
  
  FHttpClient := TFPHttpClient.Create(nil);
  FHttpClient.AddHeader('content-type', 'application/json');
  FHttpClient.AddHeader('authorization', 'Bearer ' + string(FAccessToken));
  FHttpClient.AddHeader('anthropic-version', '2023-06-01');
  FHttpClient.AddHeader('anthropic-beta', 'oauth-2025-04-20');
  
  DebugLog('TClaudeOAuthAdapter.Create: OAuth adapter created with Claude Code system message');
end;

destructor TClaudeOAuthAdapter.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TClaudeOAuthAdapter.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := 'system';
    lmrUser: Result := 'user';
    lmrAssistant: Result := 'assistant';
  else
    Result := 'user';
  end;
end;

function TClaudeOAuthAdapter.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LUsageObj: TJSONObject;
  LContentArray: TJSONArray;
  TextContent: AnsiString;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  try
    LJsonData := GetJSON(AJsonString);
  except
    on E: Exception do
    begin
      DebugLog('ParseChatCompletionResponse: JSON parse error: ' + E.Message);
      DebugLog('ParseChatCompletionResponse: Response was: ' + AJsonString);
      raise Exception.Create('Failed to parse JSON response: ' + E.Message);
    end;
  end;

  try
    if not (LJsonData is TJSONObject) then
    begin
      raise Exception.Create('Invalid JSON response');
    end;

    LJsonObject := TJSONObject(LJsonData);
    
    // Check for error response
    if LJsonObject.Find('error') <> nil then
    begin
      if LJsonObject.Objects['error'] <> nil then
      begin
        DebugLog('ParseChatCompletionResponse: API error: ' + LJsonObject.Objects['error'].Get('message', 'Unknown error'));
        raise Exception.Create('API error: ' + LJsonObject.Objects['error'].Get('message', 'Unknown error'));
      end
      else
      begin
        raise Exception.Create('Unknown API error');
      end;
    end;
    
    // Parse model and ID
    if LJsonObject.Find('model') <> nil then
      Result.Model := LJsonObject.Strings['model'];
    if LJsonObject.Find('id') <> nil then
      Result.ID := LJsonObject.Strings['id'];

    // Parse content
    TextContent := AnsiString('');
    if LJsonObject.Find('content') <> nil then
    begin
      LContentArray := LJsonObject.Arrays['content'];
      for I := 0 to LContentArray.Count - 1 do
      begin
        if LContentArray.Objects[I].Find('text') <> nil then
        begin
          if TextContent <> AnsiString('') then
            TextContent := TextContent + AnsiString(#10);
          TextContent := TextContent + AnsiString(LContentArray.Objects[I].Strings['text']);
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
      Result.Choices[0].FinishReason := AnsiString(LJsonObject.Strings['stop_reason']);

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

procedure TClaudeOAuthAdapter.ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
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
            ACallback(AnsiString(LContent), False);
          end;
        end;
      end;
    except
      on E: Exception do
        DebugLog('ParseStreamEvent: Error parsing stream event: ' + E.Message);
    end;
    
    try
      LJsonData.Free;
    except
      ; // Ignore cleanup errors
    end;
  end
  else if AEventType = 'message_stop' then
  begin
    ACallback(AnsiString(''), True);
  end;
  // Ignore other event types
end;

function TClaudeOAuthAdapter.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
  LSystemMessage: AnsiString;
begin
  DebugLog('TClaudeOAuthAdapter.ChatCompletion: Starting request');
  
  LRequestJson := TJSONObject.Create;
  try
    // Use the model that works with OAuth (claude-sonnet-4-20250514)
    LRequestJson.Add('model', 'claude-sonnet-4-20250514');
    LRequestJson.Add('max_tokens', AMaxTokens);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // For OAuth, we ONLY use the Claude Code system message
    LSystemMessage := AnsiString('You are Claude Code, Anthropic''s official CLI for Claude.');
    LMessagesArray := TJSONArray.Create;
    
    // Add our commit message instructions as the first user message
    LMessageObj := TJSONObject.Create;
    LMessageObj.Add('role', 'user');
    LMessageObj.Add('content', 'You are an expert Git user and software developer. Your task is to analyze git diffs and create detailed, well-structured commit messages. Follow these conventions:' + #10 +
      '- First line: Short title under 50 characters using imperative mood' + #10 +
      '- Second line: Leave blank' + #10 +
      '- Following lines: Detailed description explaining WHAT changed and WHY' + #10 +
      '- Use conventional commit prefixes when appropriate (feat:, fix:, docs:, refactor:, etc.)' + #10 +
      '- Include 1-2 paragraphs describing the changes in detail' + #10 +
      '- Add blank lines between paragraphs for better readability' + #10 +
      '- Explain the motivation and context behind the changes' + #10 +
      '- Mention any important implementation details or considerations' + #10 +
      '- Use imperative mood throughout (e.g., "Add feature" not "Added feature")' + #10 +
      'Return ONLY the commit message, no explanations or additional formatting.');
    LMessagesArray.Add(LMessageObj);
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our own instructions above
        Continue;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', string(LMessage.Content));
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include ONLY the Claude Code system message for OAuth
    LRequestJson.Add('system', string(LSystemMessage));
    LRequestJson.Add('messages', LMessagesArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      
      DebugLog('TClaudeOAuthAdapter.ChatCompletion: Sending POST to ' + string(FBaseUrl) + '/messages');
      
      // Debug log request
      DebugLog('[DEBUG] Claude OAuth Request: ' + LRequestJson.AsJSON);
      
      try
        LResponseText := FHttpClient.Post(string(FBaseUrl) + '/messages');
        DebugLog('TClaudeOAuthAdapter.ChatCompletion: Response received, length = ' + IntToStr(Length(LResponseText)));
        
        // Debug log raw response
        DebugLog('[DEBUG] Claude OAuth Response: ' + LResponseText);
      except
        on E: Exception do
        begin
          DebugLog('TClaudeOAuthAdapter.ChatCompletion: HTTP error: ' + E.Message);
          raise;
        end;
      end;
      
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

procedure TClaudeOAuthAdapter.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
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
  LSystemMessage: AnsiString;
  LEventType: string;
  LEventData: string;
  LResponseCode: Integer;
begin
  DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Starting streaming request');
  
  LRequestJson := TJSONObject.Create;
  try
    // Use the model that works with OAuth (claude-sonnet-4-20250514)
    LRequestJson.Add('model', 'claude-sonnet-4-20250514');
    LRequestJson.Add('max_tokens', AMaxTokens);
    LRequestJson.Add('stream', True);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // For OAuth, we ONLY use the Claude Code system message
    LSystemMessage := AnsiString('You are Claude Code, Anthropic''s official CLI for Claude.');
    LMessagesArray := TJSONArray.Create;
    
    // Add our commit message instructions as the first user message
    LMessageObj := TJSONObject.Create;
    LMessageObj.Add('role', 'user');
    LMessageObj.Add('content', 'You are an expert Git user and software developer. Your task is to analyze git diffs and create detailed, well-structured commit messages. Follow these conventions:' + #10 +
      '- First line: Short title under 50 characters using imperative mood' + #10 +
      '- Second line: Leave blank' + #10 +
      '- Following lines: Detailed description explaining WHAT changed and WHY' + #10 +
      '- Use conventional commit prefixes when appropriate (feat:, fix:, docs:, refactor:, etc.)' + #10 +
      '- Include 1-2 paragraphs describing the changes in detail' + #10 +
      '- Add blank lines between paragraphs for better readability' + #10 +
      '- Explain the motivation and context behind the changes' + #10 +
      '- Mention any important implementation details or considerations' + #10 +
      '- Use imperative mood throughout (e.g., "Add feature" not "Added feature")' + #10 +
      'Return ONLY the commit message, no explanations or additional formatting.');
    LMessagesArray.Add(LMessageObj);
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our own instructions above
        Continue;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', string(LMessage.Content));
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include ONLY the Claude Code system message for OAuth
    LRequestJson.Add('system', string(LSystemMessage));
    LRequestJson.Add('messages', LMessagesArray);

    // Create fresh client for streaming request
    LStreamClient := TFPHttpClient.Create(nil);
    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    LResponseStream := TStringStream.Create(AnsiString(''));
    try
      LStreamClient.AddHeader('content-type', 'application/json');
      LStreamClient.AddHeader('authorization', 'Bearer ' + string(FAccessToken));
      LStreamClient.AddHeader('anthropic-version', '2023-06-01');
      LStreamClient.AddHeader('anthropic-beta', 'oauth-2025-04-20');
      
      DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Set streaming headers including anthropic-beta');
      LStreamClient.RequestBody := LRequestBody;
      
      DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: About to make request with headers:');
      DebugLog('  - content-type: application/json');
      DebugLog('  - authorization: Bearer ' + string(FAccessToken));
      DebugLog('  - anthropic-version: 2023-06-01');
      DebugLog('  - anthropic-beta: oauth-2025-04-20');
      DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Full request body: ' + LRequestJson.AsJSON);
      DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Sending streaming POST to ' + string(FBaseUrl) + '/messages');
      
      try
        // Use the stream-based Post method
        LStreamClient.Post(string(FBaseUrl) + '/messages', LResponseStream);
        LResponseCode := LStreamClient.ResponseStatusCode;
        
        DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Response code = ' + IntToStr(LResponseCode));
        DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Response length = ' + IntToStr(LResponseStream.Size));
        
        if LResponseCode <> 200 then
        begin
          DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Error response: ' + LResponseStream.DataString);
          ACallback(AnsiString('Error: HTTP ' + IntToStr(LResponseCode) + ' - ' + LResponseStream.DataString), True);
          Exit;
        end;
      except
        on E: Exception do
        begin
          DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: HTTP error: ' + E.Message);
          ACallback(AnsiString('Error: ' + E.Message), True);
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
  
  DebugLog('TClaudeOAuthAdapter.ChatCompletionStream: Streaming completed');
end;

function TClaudeOAuthAdapter.GetProviderName: string;
begin
  Result := 'claude-oauth';
end;

function TClaudeOAuthAdapter.GetDefaultModel: string;
begin
  Result := 'claude-3-5-sonnet-latest';
end;

function TClaudeOAuthAdapter.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
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
    DebugLog('Tool calling iteration ' + IntToStr(i));
    
    // Make API request with tools
    Response := ChatCompletionWithToolsInternal(AModel, CurrentMessages, ATools, ATemperature, AMaxTokens);
    
    // Check if AI wants to use tools
    if Length(Response.Choices) > 0 then
    begin
      if Length(Response.Choices[0].Message.ToolCalls) > 0 then
      begin
        DebugLog('AI requested ' + IntToStr(Length(Response.Choices[0].Message.ToolCalls)) + ' tool calls');
        
        // IMPORTANT: Add assistant's message with tool calls to conversation FIRST
        SetLength(CurrentMessages, Length(CurrentMessages) + 1);
        CurrentMessages[High(CurrentMessages)] := Response.Choices[0].Message;
        
        // Execute each tool call and add results
        for LIndex := 0 to Length(Response.Choices[0].Message.ToolCalls) - 1 do
        begin
          ToolCall := Response.Choices[0].Message.ToolCalls[LIndex];
          DebugLog('Executing tool: ' + ToolCall.FunctionName);
          
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
          CurrentMessages[High(CurrentMessages)].Name := AnsiString('');
          CurrentMessages[High(CurrentMessages)].ToolCallId := ToolResult.ToolCallId;
          CurrentMessages[High(CurrentMessages)].FunctionName := ToolResult.FunctionName;
          SetLength(CurrentMessages[High(CurrentMessages)].ToolCalls, 0);
        end;
        
        // Continue conversation (loop back for next iteration)
      end
      else
      begin
        // AI provided final response, we're done
        DebugLog('Tool calling completed - final response received');
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

function TClaudeOAuthAdapter.SupportsToolCalling: Boolean;
begin
  Result := True;
end;

function TClaudeOAuthAdapter.SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
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

function TClaudeOAuthAdapter.SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
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
  
  if AParam.Description <> AnsiString('') then
    Result.Add('description', AParam.Description);
end;

function TClaudeOAuthAdapter.ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
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

function TClaudeOAuthAdapter.ChatCompletionWithToolsInternal(const AModel: string; const AMessages: array of TLLMMessage;
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
  LSystemMessage: string;
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

    // For OAuth, we ONLY use the Claude Code system message
    LSystemMessage := 'You are Claude Code, Anthropic''s official CLI for Claude.';
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        // Skip system messages for OAuth - we use our own Claude Code system message above
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
          if LMessage.Content <> AnsiString('') then
          begin
            LContentObj := TJSONObject.Create;
            LContentObj.Add('type', 'text');
            LContentObj.Add('text', string(LMessage.Content));
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
          LContentObj.Add('text', string(LMessage.Content));
          LContentArray.Add(LContentObj);
        end;
        
        LMessageObj.Add('content', LContentArray);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    // Always include the Claude Code system message for OAuth
    LRequestJson.Add('system', string(LSystemMessage));
    LRequestJson.Add('messages', LMessagesArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      
      DebugLog('TClaudeOAuthAdapter.ChatCompletionWithToolsInternal: Sending POST to ' + string(FBaseUrl) + '/messages');
      
      // Debug log request
      DebugLog('[DEBUG] Claude OAuth Tools Request: ' + LRequestJson.AsJSON);
      
      try
        LResponseText := FHttpClient.Post(string(FBaseUrl) + '/messages');
        DebugLog('TClaudeOAuthAdapter.ChatCompletionWithToolsInternal: Response received, length = ' + IntToStr(Length(LResponseText)));
        
        // Debug log raw response
        DebugLog('[DEBUG] Claude OAuth Tools Response: ' + LResponseText);
      except
        on E: Exception do
        begin
          DebugLog('TClaudeOAuthAdapter.ChatCompletionWithToolsInternal: HTTP error: ' + E.Message);
          raise;
        end;
      end;
      
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

end.