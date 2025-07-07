unit gemini_provider;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils, models;

type
  { TGeminiProvider - Google Gemini implementation }
  TGeminiProvider = class(TBaseToolProvider, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
    function SerializeGeminiTools(const ATools: TToolFunctionArray): TJSONArray;
    function SerializeGeminiToolParameter(const AParam: TToolParameter): TJSONObject;
  public
    constructor Create(const AApiKey: string; const ABaseUrl: string = 'https://generativelanguage.googleapis.com/v1beta');
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
    function ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
      const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; override;
    function SupportsToolCalling: Boolean;
  end;

implementation

{ TGeminiProvider }

function TGeminiProvider.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := AnsiString('user');     // Gemini treats system as user
    lmrUser: Result := AnsiString('user');
    lmrAssistant: Result := AnsiString('model'); // Gemini uses 'model' not 'assistant' 
    lmrTool: Result := AnsiString('user');       // Tool results are user messages
    else Result := AnsiString('user');
  end;
end;

function TGeminiProvider.StringToMessageRole(const AStr: string): TLLMMessageRole;
begin
  if AStr = 'system' then Result := lmrSystem
  else if AStr = 'user' then Result := lmrUser
  else if AStr = 'assistant' then Result := lmrAssistant
  else if AStr = 'model' then Result := lmrAssistant  // Gemini uses 'model' for assistant
  else if AStr = 'tool' then Result := lmrTool
  else Result := lmrUser;
end;

constructor TGeminiProvider.Create(const AApiKey: string; const ABaseUrl: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FBaseUrl := ABaseUrl;
  FHttpClient := TFPHttpClient.Create(nil);
  FHttpClient.AddHeader('Content-Type', 'application/json');
  FHttpClient.AddHeader('Authorization', 'Bearer ' + FApiKey);
end;

destructor TGeminiProvider.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

class function TGeminiProvider.GetApiKeyFromEnvironment: string;
begin
  Result := GetEnvironmentVariable('GEMINI_API_KEY');
  if Result = AnsiString('') then
    raise Exception.Create('GEMINI_API_KEY environment variable not set');
end;

function TGeminiProvider.GetProviderName: string;
begin
  Result := AnsiString('Gemini');
end;

function TGeminiProvider.GetDefaultModel: string;
begin
  Result := AnsiString('gemini-1.5-flash');
end;

function TGeminiProvider.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
begin
  Result := ChatCompletionWithToolsBase(AModel, AMessages, ATools, AToolContext, ATemperature, AMaxTokens);
end;

function TGeminiProvider.SupportsToolCalling: Boolean;
begin
  Result := True;
end;

function TGeminiProvider.ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
  const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
var
  LRequestJson: TJSONObject;
  LContentsArray: TJSONArray;
  LMessageObj: TJSONObject;
  LMessage: TLLMMessage;
  LRequestBody: TStringStream;
  LResponseText: string;
  LIndex: Integer;
  LToolIndex: Integer;
  LToolsArray: TJSONArray;
  LGenConfig: TJSONObject;
  LPartsArray: TJSONArray;
  LPartObj: TJSONObject;
  LTempClient: TFPHttpClient;
begin
  
  LRequestJson := TJSONObject.Create;
  try
    // Add model and generation config
    LRequestJson.Add('model', AModel);
    
    // Gemini uses generationConfig instead of top-level parameters
    if (ATemperature <> 0.7) or (AMaxTokens <> 2048) then
    begin
      LGenConfig := TJSONObject.Create;
      if ATemperature <> 0.7 then
        LGenConfig.Add('temperature', ATemperature);
      if AMaxTokens <> 2048 then
        LGenConfig.Add('maxOutputTokens', AMaxTokens);
      LRequestJson.Add('generationConfig', LGenConfig);
    end;

    // Add tools if provided (Gemini format)
    if Length(ATools) > 0 then
    begin
      LToolsArray := SerializeGeminiTools(ATools);
      LRequestJson.Add('tools', LToolsArray);
    end;

    // Add contents array (Gemini uses 'contents' not 'messages')
    LContentsArray := TJSONArray.Create;
    
    for LIndex := 0 to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      
      LMessageObj := TJSONObject.Create;
      LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
      
      // Gemini expects 'parts' array with content
      LPartsArray := TJSONArray.Create;
      
      // Check if this is a tool result message (user/tool role with ToolCallId)
      if ((LMessage.Role = lmrUser) or (LMessage.Role = lmrTool)) and (LMessage.ToolCallId <> AnsiString('')) then
      begin
        // Validate that FunctionName is set for tool results
        if LMessage.FunctionName = AnsiString('') then
        begin
          raise Exception.Create('Tool result message is missing FunctionName. ' +
                                'In your ExecuteTool method, ensure you set: Result.FunctionName := ToolCall.FunctionName;');
        end;
        
        // This is a tool result - format as functionResponse
        LPartObj := TJSONObject.Create;
        LPartObj.Add('functionResponse', TJSONObject.Create);
        LPartObj.Objects['functionResponse'].Add('name', LMessage.FunctionName);
        LPartObj.Objects['functionResponse'].Add('response', TJSONObject.Create);
        LPartObj.Objects['functionResponse'].Objects['response'].Add('content', LMessage.Content);
        LPartsArray.Add(LPartObj);
      end
      else if (LMessage.Role = lmrAssistant) and (Length(LMessage.ToolCalls) > 0) then
      begin
        // Assistant message with tool calls - format as functionCall parts
        for LToolIndex := 0 to High(LMessage.ToolCalls) do
        begin
          LPartObj := TJSONObject.Create;
          LPartObj.Add('functionCall', TJSONObject.Create);
          LPartObj.Objects['functionCall'].Add('name', LMessage.ToolCalls[LToolIndex].FunctionName);
          LPartObj.Objects['functionCall'].Add('args', TJSONObject.Create);
          LPartsArray.Add(LPartObj);
        end;
      end
      else
      begin
        // Regular text message
        LPartObj := TJSONObject.Create;
        LPartObj.Add('text', LMessage.Content);
        LPartsArray.Add(LPartObj);
      end;
      
      LMessageObj.Add('parts', LPartsArray);
      
      LContentsArray.Add(LMessageObj);
    end;
    
    LRequestJson.Add('contents', LContentsArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      // Create a fresh HTTP client without Bearer auth for native Gemini API
      LTempClient := TFPHttpClient.Create(nil);
      try
        LTempClient.AddHeader('Content-Type', 'application/json');
        LTempClient.RequestBody := LRequestBody;
        LResponseText := LTempClient.Post(FBaseUrl + '/models/' + AModel + ':generateContent?key=' + FApiKey);
      finally
        LTempClient.Free;
      end;
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TGeminiProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LChoicesArray: TJSONArray;
  LIndex: Integer;
  LChoiceObj, LMessageObj, LUsageObj: TJSONObject;
  LChoicesData, LUsageData: TJSONData;
  LErrorData: TJSONData;
  LErrorObj: TJSONObject;
begin
  
  // Initialize all fields properly - use values from actual response
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
        raise Exception.Create('Gemini API Error: ' + LErrorObj.Strings['message']);
      end
      else
        raise Exception.Create('Gemini API Error: ' + LErrorData.AsString);
    end;

    // Parse response metadata from OpenAI format
    if LJsonObject.Find('id') <> nil then
      Result.ID := LJsonObject.Strings['id']
    else
      Result.ID := AnsiString('gemini-' + IntToStr(Random(999999)));
      
    if LJsonObject.Find('object') <> nil then
      Result.ObjectStr := LJsonObject.Strings['object']
    else
      Result.ObjectStr := AnsiString('chat.completion');
      
    if LJsonObject.Find('created') <> nil then
      Result.Created := LJsonObject.Integers['created']
    else
      Result.Created := DateTimeToUnix(Now);
      
    if LJsonObject.Find('model') <> nil then
      Result.Model := LJsonObject.Strings['model']
    else
      Result.Model := AnsiString('gemini');

    // Parse OpenAI response format (uses 'choices' not 'candidates')
    LChoicesData := LJsonObject.Find('choices');
    if (LChoicesData <> nil) and (LChoicesData is TJSONArray) then
    begin
      LChoicesArray := TJSONArray(LChoicesData);
      SetLength(Result.Choices, LChoicesArray.Count);
      
      for LIndex := 0 to LChoicesArray.Count - 1 do
      begin
        if (LChoicesArray.Items[LIndex] is TJSONObject) then
        begin
          LChoiceObj := TJSONObject(LChoicesArray.Items[LIndex]);
          
          // Get index
          if LChoiceObj.Find('index') <> nil then
            Result.Choices[LIndex].Index := LChoiceObj.Integers['index']
          else
            Result.Choices[LIndex].Index := LIndex;
          
          // Get finish reason
          if LChoiceObj.Find('finish_reason') <> nil then
            Result.Choices[LIndex].FinishReason := LChoiceObj.Strings['finish_reason']
          else
            Result.Choices[LIndex].FinishReason := AnsiString('stop');

          // Parse message object
          LMessageObj := nil;
          if LChoiceObj.Find('message') <> nil then
          begin
            if LChoiceObj.Find('message') is TJSONObject then
              LMessageObj := LChoiceObj.Objects['message'];
          end;
          
          if LMessageObj <> nil then
          begin
            // Get role
            if LMessageObj.Find('role') <> nil then
              Result.Choices[LIndex].Message.Role := StringToMessageRole(LMessageObj.Strings['role'])
            else
              Result.Choices[LIndex].Message.Role := lmrAssistant;
              
            // Get content
            if LMessageObj.Find('content') <> nil then
              Result.Choices[LIndex].Message.Content := LMessageObj.Strings['content']
            else
              Result.Choices[LIndex].Message.Content := AnsiString('');
              
            // Initialize other message fields
            Result.Choices[LIndex].Message.Name := AnsiString('');
            Result.Choices[LIndex].Message.ToolCallId := AnsiString('');
            Result.Choices[LIndex].Message.FunctionName := AnsiString('');
            SetLength(Result.Choices[LIndex].Message.ToolCalls, 0);
            
            // TODO: Add tool calls parsing if needed for OpenAI format
          end;
        end;
      end;
    end;

    // Parse usage information (OpenAI format)
    LUsageData := LJsonObject.Find('usage');
    if (LUsageData <> nil) and (LUsageData is TJSONObject) then
    begin
      LUsageObj := TJSONObject(LUsageData);
      if LUsageObj.Find('prompt_tokens') <> nil then
        Result.Usage.PromptTokens := LUsageObj.Integers['prompt_tokens'];
      if LUsageObj.Find('completion_tokens') <> nil then
        Result.Usage.CompletionTokens := LUsageObj.Integers['completion_tokens'];
      if LUsageObj.Find('total_tokens') <> nil then
        Result.Usage.TotalTokens := LUsageObj.Integers['total_tokens'];
    end;

  finally
    LJsonData.Free;
  end;
end;

function TGeminiProvider.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
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
    LRequestJson.Add('temperature', ATemperature);
    LRequestJson.Add('max_tokens', AMaxTokens);

    LMessagesArray := TJSONArray.Create;
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      LMessageObj := TJSONObject.Create;
      LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
      LMessageObj.Add('content', LMessage.Content);
      if LMessage.Name <> AnsiString('') then
        LMessageObj.Add('name', LMessage.Name);
      if LMessage.ToolCallId <> AnsiString('') then
        LMessageObj.Add('tool_call_id', LMessage.ToolCallId);
      LMessagesArray.Add(LMessageObj);
    end;
    LRequestJson.Add('messages', LMessagesArray);

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      LResponseText := FHttpClient.Post(FBaseUrl + '/chat/completions');
      
      // Check for HTTP error status
      if FHttpClient.ResponseStatusCode <> 200 then
      begin
        raise Exception.Create('Gemini API HTTP Error ' + IntToStr(FHttpClient.ResponseStatusCode) + ': ' + LResponseText);
      end;
      
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

procedure TGeminiProvider.ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LChoicesArray: TJSONArray;
  LChoiceObj: TJSONObject;
  LDeltaObj: TJSONObject;
  LContent: string;
begin
  // Skip empty lines and non-data lines
  if (Trim(ALine) = AnsiString('')) or (not ALine.StartsWith('data: ')) then
    Exit;
    
  // Remove 'data: ' prefix
  LContent := Copy(ALine, 7, Length(ALine) - 6);
  
  // Check for [DONE] message
  if LContent = '[DONE]' then
  begin
    ACallback(AnsiString(''), True);
    Exit;
  end;
  
  try
    LJsonData := GetJSON(LContent);
    if not (LJsonData is TJSONObject) then
      Exit;
      
    LJsonObject := TJSONObject(LJsonData);
    
    // Parse choices array (OpenAI compatible format)
    LChoicesArray := LJsonObject.Arrays['choices'];
    if (LChoicesArray <> nil) and (LChoicesArray.Count > 0) then
    begin
      LChoiceObj := TJSONObject(LChoicesArray.Items[0]);
      LDeltaObj := LChoiceObj.Objects['delta'];
      if LDeltaObj <> nil then
      begin
        if LDeltaObj.Find('content') <> nil then
        begin
          LContent := LDeltaObj.Strings['content'];
          ACallback(LContent, False);
        end;
      end;
    end;
    
  except
    on E: Exception do
      ; // Ignore parsing errors for invalid chunks
  end;
  
  try
    LJsonData.Free;
  except
    ; // Ignore cleanup errors
  end;
end;

procedure TGeminiProvider.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
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
begin
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('temperature', ATemperature);
    LRequestJson.Add('max_tokens', AMaxTokens);
    LRequestJson.Add('stream', True);

    LMessagesArray := TJSONArray.Create;
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      LMessageObj := TJSONObject.Create;
      LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
      LMessageObj.Add('content', LMessage.Content);
      if LMessage.Name <> AnsiString('') then
        LMessageObj.Add('name', LMessage.Name);
      if LMessage.ToolCallId <> AnsiString('') then
        LMessageObj.Add('tool_call_id', LMessage.ToolCallId);
      LMessagesArray.Add(LMessageObj);
    end;
    LRequestJson.Add('messages', LMessagesArray);

    // Create fresh client for streaming request
    LStreamClient := TFPHttpClient.Create(nil);
    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    LResponseStream := TStringStream.Create(AnsiString(''));
    try
      LStreamClient.AddHeader('Content-Type', 'application/json');
      LStreamClient.AddHeader('Authorization', 'Bearer ' + FApiKey);
      LStreamClient.RequestBody := LRequestBody;
      
      // Use the stream-based Post method
      LStreamClient.Post(FBaseUrl + '/chat/completions', LResponseStream);
      
      // Check for HTTP error status
      if LStreamClient.ResponseStatusCode <> 200 then
      begin
        raise Exception.Create('Gemini API HTTP Error ' + IntToStr(LStreamClient.ResponseStatusCode) + ': ' + LResponseStream.DataString);
      end;
      
      // Parse the response line by line
      LResponseLines := TStringList.Create;
      try
        LResponseLines.Text := LResponseStream.DataString;
        for LIndex := 0 to LResponseLines.Count - 1 do
        begin
          LLine := LResponseLines.Strings[LIndex];
          ParseStreamChunk(LLine, ACallback);
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

function TGeminiProvider.SerializeGeminiTools(const ATools: TToolFunctionArray): TJSONArray;
var
  LIndex: Integer;
  LTool: TToolFunction;
  LToolObj: TJSONObject;
  LFunctionDeclarationsArray: TJSONArray;
  LFunctionDeclaration: TJSONObject;
  LParametersObj: TJSONObject;
  LPropertiesObj: TJSONObject;
  LRequiredArray: TJSONArray;
  LParamIndex: Integer;
  LParam: TToolParameter;
begin
  Result := TJSONArray.Create;
  
  // Create single tool object with functionDeclarations array
  LToolObj := TJSONObject.Create;
  LFunctionDeclarationsArray := TJSONArray.Create;
  
  for LIndex := 0 to High(ATools) do
  begin
    LTool := ATools[LIndex];
    
    // Create function declaration object
    LFunctionDeclaration := TJSONObject.Create;
    
    LFunctionDeclaration.Add('name', LTool.Name);
    LFunctionDeclaration.Add('description', LTool.Description);
    
    // Add parameters schema only if function has parameters
    if Length(LTool.Parameters) > 0 then
    begin
      LParametersObj := TJSONObject.Create;
      LParametersObj.Add('type', 'OBJECT');
      
      LPropertiesObj := TJSONObject.Create;
      LRequiredArray := TJSONArray.Create;
      
      for LParamIndex := 0 to High(LTool.Parameters) do
      begin
        LParam := LTool.Parameters[LParamIndex];
        LPropertiesObj.Add(LParam.Name, SerializeGeminiToolParameter(LParam));
        
        if LParam.Required then
          LRequiredArray.Add(LParam.Name);
      end;
      
      LParametersObj.Add('properties', LPropertiesObj);
      if LRequiredArray.Count > 0 then
        LParametersObj.Add('required', LRequiredArray)
      else
        LRequiredArray.Free;
      
      LFunctionDeclaration.Add('parameters', LParametersObj);
    end;
    
    // Add to functionDeclarations array
    LFunctionDeclarationsArray.Add(LFunctionDeclaration);
  end;
  
  // Add functionDeclarations array to tool object
  LToolObj.Add('functionDeclarations', LFunctionDeclarationsArray);  // Fixed: functionDeclarations not function_declaration
  Result.Add(LToolObj);
  
end;

function TGeminiProvider.SerializeGeminiToolParameter(const AParam: TToolParameter): TJSONObject;
var
  EnumArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  
  // Map parameter type to JSON schema type (Gemini uses uppercase)
  case AParam.ParamType of
    tptString: Result.Add('type', 'STRING');
    tptInteger: Result.Add('type', 'INTEGER');
    tptNumber: Result.Add('type', 'NUMBER');
    tptBoolean: Result.Add('type', 'BOOLEAN');
    tptArray: Result.Add('type', 'ARRAY');
    tptObject: Result.Add('type', 'OBJECT');
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

end.
