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
  LCandidatesArray: TJSONArray;
  LIndex, LPartIndex: Integer;
  LCandidateObj, LContentObj, LPartObj, LUsageObj: TJSONObject;
  LCandidatesData, LUsageData: TJSONData;
  LErrorData: TJSONData;
  LErrorObj: TJSONObject;
  LPartsArray: TJSONArray;
  LFunctionCallsCount: Integer;
  LToolCallsArray: TToolCallArray;
  LFunctionCallObj: TJSONObject;
  LArgsObj: TJSONObject;
  LTextContent: string;
begin
  
  // Initialize all fields properly
  Result.ID := AnsiString('gemini-' + IntToStr(Random(999999)));
  Result.ObjectStr := AnsiString('chat.completion');
  Result.Created := DateTimeToUnix(Now);
  Result.Model := AnsiString('gemini');
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

    // Parse Gemini response format (uses 'candidates' not 'choices')
    LCandidatesData := LJsonObject.Find('candidates');
    if (LCandidatesData <> nil) and (LCandidatesData is TJSONArray) then
    begin
      LCandidatesArray := TJSONArray(LCandidatesData);
      SetLength(Result.Choices, LCandidatesArray.Count);
      
      for LIndex := 0 to LCandidatesArray.Count - 1 do
      begin
        if (LCandidatesArray.Items[LIndex] is TJSONObject) then
        begin
          LCandidateObj := TJSONObject(LCandidatesArray.Items[LIndex]);
          Result.Choices[LIndex].Index := LIndex;
          
          // Get finish reason if available
          if LCandidateObj.Find('finishReason') <> nil then
            Result.Choices[LIndex].FinishReason := LCandidateObj.Strings['finishReason']
          else
            Result.Choices[LIndex].FinishReason := AnsiString('stop');

          // Parse content object safely
          LContentObj := nil;
          if LCandidateObj.Find('content') <> nil then
          begin
            if LCandidateObj.Find('content') is TJSONObject then
              LContentObj := LCandidateObj.Objects['content'];
          end;
          
          if LContentObj <> nil then
          begin
            Result.Choices[LIndex].Message.Role := lmrAssistant;
            Result.Choices[LIndex].Message.Name := AnsiString('');
            Result.Choices[LIndex].Message.ToolCallId := AnsiString('');
            
            // Parse parts array for text content and function calls
            // Handle both array and non-array cases for 'parts' field
            LPartsArray := nil;
            if LContentObj.Find('parts') <> nil then
            begin
              if LContentObj.Find('parts') is TJSONArray then
                LPartsArray := LContentObj.Arrays['parts'];
            end;
            
            if LPartsArray <> nil then
            begin
              LTextContent := AnsiString('');
              LFunctionCallsCount := 0;
              SetLength(LToolCallsArray, 0);
              
              // First pass: count function calls and collect text
              for LPartIndex := 0 to LPartsArray.Count - 1 do
              begin
                if LPartsArray.Items[LPartIndex] is TJSONObject then
                begin
                  LPartObj := TJSONObject(LPartsArray.Items[LPartIndex]);
                  
                  // Check for text content
                  if LPartObj.Find('text') <> nil then
                  begin
                    if LTextContent <> AnsiString('') then
                      LTextContent := LTextContent + ' ';
                    LTextContent := LTextContent + LPartObj.Strings['text'];
                  end;
                  
                  // Check for function call
                  if LPartObj.Find('functionCall') <> nil then
                    Inc(LFunctionCallsCount);
                end;
              end;
              
              Result.Choices[LIndex].Message.Content := LTextContent;
              
              // Second pass: extract function calls
              if LFunctionCallsCount > 0 then
              begin
                SetLength(LToolCallsArray, LFunctionCallsCount);
                LFunctionCallsCount := 0; // Reset for indexing
                
                for LPartIndex := 0 to LPartsArray.Count - 1 do
                begin
                  if LPartsArray.Items[LPartIndex] is TJSONObject then
                  begin
                    LPartObj := TJSONObject(LPartsArray.Items[LPartIndex]);
                    
                    if LPartObj.Find('functionCall') <> nil then
                    begin
                      LFunctionCallObj := LPartObj.Objects['functionCall'];
                      if LFunctionCallObj <> nil then
                      begin
                        LToolCallsArray[LFunctionCallsCount].Id := 'call_' + IntToStr(Random(999999));
                        LToolCallsArray[LFunctionCallsCount].FunctionName := LFunctionCallObj.Strings['name'];
                        
                        // Convert args object to JSON string
                        LArgsObj := LFunctionCallObj.Objects['args'];
                        if LArgsObj <> nil then
                          LToolCallsArray[LFunctionCallsCount].Arguments := LArgsObj.AsJSON
                        else
                          LToolCallsArray[LFunctionCallsCount].Arguments := AnsiString('{}');
                        
                        Inc(LFunctionCallsCount);
                      end;
                    end;
                  end;
                end;
              end;
              
              SetLength(Result.Choices[LIndex].Message.ToolCalls, Length(LToolCallsArray));
              for LPartIndex := 0 to High(LToolCallsArray) do
                Result.Choices[LIndex].Message.ToolCalls[LPartIndex] := LToolCallsArray[LPartIndex];
            end
            else if LContentObj.Find('parts') <> nil then
            begin
              // Handle case where 'parts' exists but is not an array
              // Treat it as a single object and extract text if available
              if LContentObj.Find('parts') is TJSONObject then
              begin
                LPartObj := TJSONObject(LContentObj.Find('parts'));
                if LPartObj.Find('text') <> nil then
                  Result.Choices[LIndex].Message.Content := LPartObj.Strings['text']
                else
                  Result.Choices[LIndex].Message.Content := AnsiString('');
              end
              else
              begin
                // If parts is neither array nor object, try to use as string
                Result.Choices[LIndex].Message.Content := LContentObj.Find('parts').AsString;
              end;
            end;
          end;
        end;
      end;
    end;

    // Parse usage metadata if available
    LUsageData := LJsonObject.Find('usageMetadata');
    if (LUsageData <> nil) and (LUsageData is TJSONObject) then
    begin
      LUsageObj := TJSONObject(LUsageData);
      if LUsageObj.Find('promptTokenCount') <> nil then
        Result.Usage.PromptTokens := LUsageObj.Integers['promptTokenCount'];
      if LUsageObj.Find('candidatesTokenCount') <> nil then
        Result.Usage.CompletionTokens := LUsageObj.Integers['candidatesTokenCount'];
      if LUsageObj.Find('totalTokenCount') <> nil then
        Result.Usage.TotalTokens := LUsageObj.Integers['totalTokenCount'];
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
