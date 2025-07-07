unit claude_provider;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils, models;

type
  { TClaudeProvider - Anthropic Claude implementation }
  TClaudeProvider = class(TBaseToolProvider, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
    
    // Tool calling helper methods
    function SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
    function SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
    function ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
    function CreateToolResultMessage(const AToolResult: TToolResult): TJSONObject;
  protected
    // Override for base tool provider
    function ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
      const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; override;
  public
    constructor Create(const AApiKey: string; const ABaseUrl: string = 'https://api.anthropic.com/v1');
    destructor Destroy; override;
    function ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    procedure ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
      ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
    function GetProviderName: string;
    function GetDefaultModel: string;
    class function GetApiKeyFromEnvironment: string;
    
    // Tool calling methods
    function ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
      const ATools: array of TToolFunction; AToolContext: IToolContext;
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    function SupportsToolCalling: Boolean;
  end;

implementation

{ TClaudeProvider }

function TClaudeProvider.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := AnsiString('system');
    lmrUser: Result := AnsiString('user');
    lmrAssistant: Result := AnsiString('assistant');
    lmrTool: Result := AnsiString('tool');
    else Result := AnsiString('');
  end;
end;

function TClaudeProvider.StringToMessageRole(const AStr: string): TLLMMessageRole;
begin
  if AStr = 'system' then Result := lmrSystem
  else if AStr = 'user' then Result := lmrUser
  else if AStr = 'assistant' then Result := lmrAssistant
  else if AStr = 'tool' then Result := lmrTool
  else Result := lmrUser;
end;

constructor TClaudeProvider.Create(const AApiKey: string; const ABaseUrl: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FBaseUrl := ABaseUrl;
  FHttpClient := TFPHttpClient.Create(nil);
  FHttpClient.AddHeader('Content-Type', 'application/json');
  FHttpClient.AddHeader('x-api-key', FApiKey);
  FHttpClient.AddHeader('anthropic-version', '2023-06-01');
end;

destructor TClaudeProvider.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

class function TClaudeProvider.GetApiKeyFromEnvironment: string;
begin
  Result := GetEnvironmentVariable('ANTHROPIC_API_KEY');
  if Result = AnsiString('') then
    raise Exception.Create('ANTHROPIC_API_KEY environment variable not set');
end;

function TClaudeProvider.GetProviderName: string;
begin
  Result := AnsiString('Claude');
end;

function TClaudeProvider.GetDefaultModel: string;
begin
  Result := AnsiString('claude-3-haiku-20240307');
end;

function TClaudeProvider.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
begin
  Result := ChatCompletionWithToolsBase(AModel, AMessages, ATools, AToolContext, ATemperature, AMaxTokens);
end;

function TClaudeProvider.SupportsToolCalling: Boolean;
begin
  Result := True;
end;

function TClaudeProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LErrorData: TJSONData;
  LErrorObj: TJSONObject;
  LContentArray: TJSONArray;
  LContentItem: TJSONObject;
  LUsageObj: TJSONObject;
  i: Integer;
  TextContent: string;
begin
  
  // Initialize all fields properly
  Result.ID := AnsiString('');
  Result.ObjectStr := AnsiString('');
  Result.Created := 0;
  Result.Model := AnsiString('');
  SetLength(Result.Choices, 0);
  Result.Usage.PromptTokens := 0;
  Result.Usage.CompletionTokens := 0;
  Result.Usage.TotalTokens := 0;

  try
    LJsonData := GetJSON(AJsonString);
    if not (LJsonData is TJSONObject) then
      raise Exception.Create('Invalid JSON response: Not an object.');

    LJsonObject := TJSONObject(LJsonData);

    // Check if this is an error response
    LErrorData := LJsonObject.Find('error');
    if LErrorData <> nil then
    begin
      if LErrorData is TJSONObject then
      begin
        LErrorObj := TJSONObject(LErrorData);
        raise Exception.Create('Claude API Error: ' + LErrorObj.Strings['message']);
      end
      else
        raise Exception.Create('Claude API Error: ' + LErrorData.AsString);
    end;

    // Parse Claude response format
    if LJsonObject.Find('id') <> nil then
      Result.ID := LJsonObject.Strings['id'];
    if LJsonObject.Find('type') <> nil then
      Result.ObjectStr := LJsonObject.Strings['type'];
    if LJsonObject.Find('model') <> nil then
      Result.Model := LJsonObject.Strings['model'];
      
    // Claude uses 'content' array instead of 'choices'
    LContentArray := LJsonObject.Arrays['content'];
    if LContentArray <> nil then
    begin
      SetLength(Result.Choices, 1);
      Result.Choices[0].Index := 0;
      Result.Choices[0].Message.Role := lmrAssistant;
      Result.Choices[0].Message.Content := AnsiString('');
      Result.Choices[0].Message.Name := AnsiString('');
      Result.Choices[0].Message.ToolCallId := AnsiString('');
      
      // Parse content blocks and collect tool calls
      Result.Choices[0].Message.ToolCalls := ParseToolUseFromContent(LContentArray);
      
      // Combine text content from all text blocks
      TextContent := AnsiString('');
      for i := 0 to LContentArray.Count - 1 do
      begin
        if LContentArray.Items[i] is TJSONObject then
        begin
          LContentItem := TJSONObject(LContentArray.Items[i]);
          if LContentItem.Strings['type'] = 'text' then
          begin
            if TextContent <> AnsiString('') then
              TextContent := TextContent + ' ';
            TextContent := TextContent + LContentItem.Strings['text'];
          end;
        end;
      end;
      Result.Choices[0].Message.Content := TextContent;
      
      if LJsonObject.Find('stop_reason') <> nil then
        Result.Choices[0].FinishReason := LJsonObject.Strings['stop_reason'];
    end;

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

function TClaudeProvider.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
  LSystemMessage: string;
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('max_tokens', AMaxTokens);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // Claude separates system message from conversation messages
    LSystemMessage := AnsiString('');
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        if LSystemMessage <> AnsiString('') then
          LSystemMessage := LSystemMessage + ' ' + LMessage.Content
        else
          LSystemMessage := LMessage.Content;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', LMessage.Content);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    if LSystemMessage <> AnsiString('') then
      LRequestJson.Add('system', LSystemMessage);
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

procedure TClaudeProvider.ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
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
      on E: Exception do
        ; // Ignore parsing errors
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
  // Ignore other event types (message_start, content_block_start, etc.)
end;

procedure TClaudeProvider.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
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
  LSystemMessage: string;
  LEventType: string;
  LEventData: string;
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('max_tokens', AMaxTokens);
    LRequestJson.Add('stream', True);
    if ATemperature <> 0.7 then
      LRequestJson.Add('temperature', ATemperature);

    // Claude separates system message from conversation messages
    LSystemMessage := AnsiString('');
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        if LSystemMessage <> AnsiString('') then
          LSystemMessage := LSystemMessage + ' ' + LMessage.Content
        else
          LSystemMessage := LMessage.Content;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        LMessageObj.Add('content', LMessage.Content);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    if LSystemMessage <> AnsiString('') then
      LRequestJson.Add('system', LSystemMessage);
    LRequestJson.Add('messages', LMessagesArray);

    // Create fresh client for streaming request
    LStreamClient := TFPHttpClient.Create(nil);
    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    LResponseStream := TStringStream.Create(AnsiString(''));
    try
      LStreamClient.AddHeader('Content-Type', 'application/json');
      LStreamClient.AddHeader('x-api-key', FApiKey);
      LStreamClient.AddHeader('anthropic-version', '2023-06-01');
      LStreamClient.RequestBody := LRequestBody;
      
      // Use the stream-based Post method like the docs example
      LStreamClient.Post(FBaseUrl + '/messages', LResponseStream);
      
      // Parse the response line by line for SSE events
      LResponseLines := TStringList.Create;
      try
        LResponseLines.Text := LResponseStream.DataString;
        LEventType := AnsiString('');
        LEventData := AnsiString('');
        
        for LIndex := 0 to LResponseLines.Count - 1 do
        begin
          LLine := Trim(LResponseLines.Strings[LIndex]);
          
          if LLine.StartsWith('event: ') then
          begin
            LEventType := Copy(LLine, 8, Length(LLine) - 7);
          end
          else if LLine.StartsWith('data: ') then
          begin
            LEventData := Copy(LLine, 7, Length(LLine) - 6);
            if LEventType <> AnsiString('') then
            begin
              ParseStreamEvent(LEventType, LEventData, ACallback);
              LEventType := AnsiString('');
              LEventData := AnsiString('');
            end;
          end;
        end;
      finally
        LResponseLines.Free;
      end;
      
    finally
      LRequestBody.Free;
      LResponseStream.Free;
      LStreamClient.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TClaudeProvider.ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
  const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LMessagesArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
  LToolsArray: TJSONArray;
  LSystemMessage: string;
  LContentArray: TJSONArray;
  LContentObj: TJSONObject;
  ArgsJson: TJSONData;
  ToolCallIndex: Integer;
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

    // Claude separates system message from conversation messages
    LSystemMessage := AnsiString('');
    LMessagesArray := TJSONArray.Create;
    
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      if LMessage.Role = lmrSystem then
      begin
        if LSystemMessage <> AnsiString('') then
          LSystemMessage := LSystemMessage + ' ' + LMessage.Content
        else
          LSystemMessage := LMessage.Content;
      end
      else
      begin
        LMessageObj := TJSONObject.Create;
        LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
        
        // Claude uses content blocks
        LContentArray := TJSONArray.Create;
        
        // Check if this is a user message that's actually a tool result
        if (LMessage.Role = lmrUser) and (LMessage.ToolCallId <> AnsiString('')) then
        begin
          // Tool result message (user message with tool_result content block)
          LContentObj := TJSONObject.Create;
          LContentObj.Add('type', 'tool_result');
          LContentObj.Add('tool_use_id', LMessage.ToolCallId);
          LContentObj.Add('content', LMessage.Content);
          LContentArray.Add(LContentObj);
        end
        else if LMessage.Role = lmrTool then
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
            LContentObj.Add('text', LMessage.Content);
            LContentArray.Add(LContentObj);
          end;
          
          // Add tool_use blocks
          for ToolCallIndex := 0 to High(LMessage.ToolCalls) do
          begin
            LContentObj := TJSONObject.Create;
            LContentObj.Add('type', 'tool_use');
            LContentObj.Add('id', LMessage.ToolCalls[ToolCallIndex].Id);
            LContentObj.Add('name', LMessage.ToolCalls[ToolCallIndex].FunctionName);
            // Parse arguments JSON and add as object
            ArgsJson := GetJSON(LMessage.ToolCalls[ToolCallIndex].Arguments);
            LContentObj.Add('input', ArgsJson);
            LContentArray.Add(LContentObj);
          end;
        end
        else
        begin
          // Regular text message (user or assistant without tool calls)
          LContentObj := TJSONObject.Create;
          LContentObj.Add('type', 'text');
          LContentObj.Add('text', LMessage.Content);
          LContentArray.Add(LContentObj);
        end;
        
        LMessageObj.Add('content', LContentArray);
        LMessagesArray.Add(LMessageObj);
      end;
    end;
    
    if LSystemMessage <> AnsiString('') then
    begin
      LRequestJson.Add('system', LSystemMessage);
    end;
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

function TClaudeProvider.SerializeClaudeTools(const ATools: array of TToolFunction): TJSONArray;
var
  i: Integer;
  ToolObj: TJSONObject;
  InputSchemaObj, PropertiesObj: TJSONObject;
  RequiredArray: TJSONArray;
  j: Integer;
begin
  Result := TJSONArray.Create;
  
  for i := 0 to High(ATools) do
  begin
    ToolObj := TJSONObject.Create;
    ToolObj.Add('name', ATools[i].Name);
    ToolObj.Add('description', ATools[i].Description);
    
    // Create input_schema (Claude format)
    InputSchemaObj := TJSONObject.Create;
    InputSchemaObj.Add('type', 'object');
    
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

function TClaudeProvider.SerializeClaudeToolParameter(const AParam: TToolParameter): TJSONObject;
var
  EnumArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  
  case AParam.ParamType of
    tptString: Result.Add('type', 'string');
    tptInteger: Result.Add('type', 'integer');
    tptNumber: Result.Add('type', 'number');
    tptBoolean: Result.Add('type', 'boolean');
    tptArray: Result.Add('type', 'array');
    tptObject: Result.Add('type', 'object');
  end;
  
  if AParam.Description <> AnsiString('') then
    Result.Add('description', AParam.Description);
    
  // Add enum values if specified
  if Length(AParam.EnumValues) > 0 then
  begin
    EnumArray := TJSONArray.Create;
    for i := 0 to High(AParam.EnumValues) do
      EnumArray.Add(AParam.EnumValues[i]);
    Result.Add('enum', EnumArray);
  end;
end;

function TClaudeProvider.ParseToolUseFromContent(const AContentArray: TJSONArray): TToolCallArray;
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
        Result[ToolIndex].Arguments := ContentItem.Objects['input'].AsJSON;
        Inc(ToolIndex);
      end;
    end;
  end;
end;

function TClaudeProvider.CreateToolResultMessage(const AToolResult: TToolResult): TJSONObject;
var
  ContentArray: TJSONArray;
  ToolResultObj: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('role', 'user');
  
  ContentArray := TJSONArray.Create;
  ToolResultObj := TJSONObject.Create;
  ToolResultObj.Add('type', 'tool_result');
  ToolResultObj.Add('tool_use_id', AToolResult.ToolCallId);
  ToolResultObj.Add('content', AToolResult.Content);
  if AToolResult.IsError then
    ToolResultObj.Add('is_error', True);
  
  ContentArray.Add(ToolResultObj);
  Result.Add('content', ContentArray);
end;

end.
