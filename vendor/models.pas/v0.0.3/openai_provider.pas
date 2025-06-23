unit openai_provider;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, DateUtils, models;

type
  { TOpenAIProvider - OpenAI implementation }
  TOpenAIProvider = class(TBaseToolProvider, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
    
    // Tool calling helper methods
    function SerializeToolFunctions(const ATools: array of TToolFunction): TJSONArray;
    function SerializeToolParameter(const AParam: TToolParameter): TJSONObject;
    function ParseToolCallsFromResponse(const AJsonObject: TJSONObject): TToolCallArray;
  protected
    // Override for base tool provider
    function ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
      const ATools: TToolFunctionArray; ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse; override;
  public
    constructor Create(const AApiKey: string; const ABaseUrl: string = 'https://api.openai.com/v1');
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

{ TOpenAIProvider }

function TOpenAIProvider.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := AnsiString('system');
    lmrUser: Result := AnsiString('user');
    lmrAssistant: Result := AnsiString('assistant');
    lmrTool: Result := AnsiString('tool');
    else Result := AnsiString('');
  end;
end;

function TOpenAIProvider.StringToMessageRole(const AStr: string): TLLMMessageRole;
begin
  if AStr = 'system' then Result := lmrSystem
  else if AStr = 'user' then Result := lmrUser
  else if AStr = 'assistant' then Result := lmrAssistant
  else if AStr = 'tool' then Result := lmrTool
  else Result := lmrUser;
end;

constructor TOpenAIProvider.Create(const AApiKey: string; const ABaseUrl: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FBaseUrl := ABaseUrl;
  FHttpClient := TFPHttpClient.Create(nil);
  FHttpClient.AddHeader('Content-Type', 'application/json');
  FHttpClient.AddHeader('Authorization', 'Bearer ' + FApiKey);
end;

destructor TOpenAIProvider.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

class function TOpenAIProvider.GetApiKeyFromEnvironment: string;
begin
  Result := GetEnvironmentVariable('OPENAI_API_KEY');
  if Result = AnsiString('') then
    raise Exception.Create('OPENAI_API_KEY environment variable not set');
end;

function TOpenAIProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LJsonArray: TJSONArray;
  LIndex: Integer;
  LChoiceObj, LMessageObj, LUsageObj: TJSONObject;
  LChoicesData, LUsageData: TJSONData;
  LErrorData: TJSONData;
  LErrorObj: TJSONObject;
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
        raise Exception.Create('OpenAI API Error: ' + LErrorObj.Strings['message']);
      end
      else
        raise Exception.Create('OpenAI API Error: ' + LErrorData.AsString);
    end;

    // Parse successful response
    if LJsonObject.Find('id') <> nil then
      Result.ID := LJsonObject.Strings['id'];
    if LJsonObject.Find('object') <> nil then
      Result.ObjectStr := LJsonObject.Strings['object'];
    if LJsonObject.Find('created') <> nil then
      Result.Created := LJsonObject.Int64s['created'];
    if LJsonObject.Find('model') <> nil then
      Result.Model := LJsonObject.Strings['model'];

    LChoicesData := LJsonObject.Find('choices');
    if (LChoicesData <> nil) and (LChoicesData is TJSONArray) then
    begin
      LJsonArray := TJSONArray(LChoicesData);
      SetLength(Result.Choices, LJsonArray.Count);
      for LIndex := 0 to LJsonArray.Count - 1 do
      begin
        if (LJsonArray.Items[LIndex] is TJSONObject) then
        begin
          LChoiceObj := TJSONObject(LJsonArray.Items[LIndex]);
          Result.Choices[LIndex].Index := LChoiceObj.Integers['index'];
          Result.Choices[LIndex].FinishReason := LChoiceObj.Strings['finish_reason'];

          LMessageObj := LChoiceObj.Objects['message'];
          if LMessageObj <> nil then
          begin
            Result.Choices[LIndex].Message.Role := StringToMessageRole(LMessageObj.Strings['role']);
            if LMessageObj.Find('content') <> nil then
              Result.Choices[LIndex].Message.Content := LMessageObj.Strings['content']
            else
              Result.Choices[LIndex].Message.Content := AnsiString('');
            if LMessageObj.Find('name') <> nil then
              Result.Choices[LIndex].Message.Name := LMessageObj.Strings['name']
            else
              Result.Choices[LIndex].Message.Name := AnsiString('');
            if LMessageObj.Find('tool_call_id') <> nil then
              Result.Choices[LIndex].Message.ToolCallId := LMessageObj.Strings['tool_call_id']
            else
              Result.Choices[LIndex].Message.ToolCallId := AnsiString('');
            
            // Parse tool calls if present
            Result.Choices[LIndex].Message.ToolCalls := ParseToolCallsFromResponse(LMessageObj);
          end;
        end;
      end;
    end;

    LUsageData := LJsonObject.Find('usage');
    if (LUsageData <> nil) and (LUsageData is TJSONObject) then
    begin
      LUsageObj := TJSONObject(LUsageData);
      Result.Usage.PromptTokens := LUsageObj.Integers['prompt_tokens'];
      Result.Usage.CompletionTokens := LUsageObj.Integers['completion_tokens'];
      Result.Usage.TotalTokens := LUsageObj.Integers['total_tokens'];
    end;

  finally
    LJsonData.Free;
  end;
end;

function TOpenAIProvider.ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
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
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TOpenAIProvider.GetProviderName: string;
begin
  Result := AnsiString('OpenAI');
end;

function TOpenAIProvider.GetDefaultModel: string;
begin
  Result := AnsiString('gpt-3.5-turbo');
end;

function TOpenAIProvider.ChatCompletionWithTools(const AModel: string; const AMessages: array of TLLMMessage;
  const ATools: array of TToolFunction; AToolContext: IToolContext;
  ATemperature: Double; AMaxTokens: Integer): TLLMChatCompletionResponse;
begin
  Result := ChatCompletionWithToolsBase(AModel, AMessages, ATools, AToolContext, ATemperature, AMaxTokens);
end;

function TOpenAIProvider.SupportsToolCalling: Boolean;
begin
  Result := True;
end;

function TOpenAIProvider.ChatCompletionInternal(const AModel: string; const AMessages: TLLMMessageArray;
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
begin
  
  LRequestJson := TJSONObject.Create;
  try
    LRequestJson.Add('model', AModel);
    LRequestJson.Add('temperature', ATemperature);
    LRequestJson.Add('max_tokens', AMaxTokens);

    // Add messages array
    LMessagesArray := TJSONArray.Create;
    for LIndex := Low(AMessages) to High(AMessages) do
    begin
      LMessage := AMessages[LIndex];
      LMessageObj := TJSONObject.Create;
      LMessageObj.Add('role', MessageRoleToString(LMessage.Role));
      
      if LMessage.Content <> AnsiString('') then
        LMessageObj.Add('content', LMessage.Content);
        
      if LMessage.Name <> AnsiString('') then
        LMessageObj.Add('name', LMessage.Name);
        
      if LMessage.ToolCallId <> AnsiString('') then
        LMessageObj.Add('tool_call_id', LMessage.ToolCallId);
        
      // Add tool calls for assistant messages
      if (LMessage.Role = lmrAssistant) and (Length(LMessage.ToolCalls) > 0) then
      begin
        LToolsArray := TJSONArray.Create;
        // TODO: Serialize tool calls from message - will implement when needed
        LMessageObj.Add('tool_calls', LToolsArray);
      end;
      
      LMessagesArray.Add(LMessageObj);
    end;
    LRequestJson.Add('messages', LMessagesArray);

    // Add tools if provided
    if Length(ATools) > 0 then
    begin
      LToolsArray := SerializeToolFunctions(ATools);
      LRequestJson.Add('tools', LToolsArray);
    end;

    LRequestBody := TStringStream.Create(LRequestJson.AsJSON);
    try
      FHttpClient.RequestBody := LRequestBody;
      LResponseText := FHttpClient.Post(FBaseUrl + '/chat/completions');
      Result := ParseChatCompletionResponse(LResponseText);
    finally
      LRequestBody.Free;
    end;
  finally
    LRequestJson.Free;
  end;
end;

function TOpenAIProvider.SerializeToolFunctions(const ATools: array of TToolFunction): TJSONArray;
var
  i: Integer;
  ToolObj, FunctionObj, ParametersObj, PropertiesObj: TJSONObject;
  RequiredArray: TJSONArray;
  j: Integer;
begin
  Result := TJSONArray.Create;
  
  for i := 0 to High(ATools) do
  begin
    ToolObj := TJSONObject.Create;
    ToolObj.Add('type', 'function');
    
    FunctionObj := TJSONObject.Create;
    FunctionObj.Add('name', ATools[i].Name);
    FunctionObj.Add('description', ATools[i].Description);
    
    // Create parameters schema
    ParametersObj := TJSONObject.Create;
    ParametersObj.Add('type', 'object');
    
    PropertiesObj := TJSONObject.Create;
    RequiredArray := TJSONArray.Create;
    
    for j := 0 to High(ATools[i].Parameters) do
    begin
      PropertiesObj.Add(ATools[i].Parameters[j].Name, SerializeToolParameter(ATools[i].Parameters[j]));
      if ATools[i].Parameters[j].Required then
        RequiredArray.Add(ATools[i].Parameters[j].Name);
    end;
    
    ParametersObj.Add('properties', PropertiesObj);
    ParametersObj.Add('required', RequiredArray);
    
    FunctionObj.Add('parameters', ParametersObj);
    ToolObj.Add('function', FunctionObj);
    
    Result.Add(ToolObj);
  end;
end;

function TOpenAIProvider.SerializeToolParameter(const AParam: TToolParameter): TJSONObject;
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

function TOpenAIProvider.ParseToolCallsFromResponse(const AJsonObject: TJSONObject): TToolCallArray;
var
  ToolCallsArray: TJSONArray;
  ToolCallObj, FunctionObj: TJSONObject;
  i: Integer;
begin
  SetLength(Result, 0);
  
  ToolCallsArray := AJsonObject.Arrays['tool_calls'];
  if ToolCallsArray = nil then Exit;
  
  SetLength(Result, ToolCallsArray.Count);
  for i := 0 to ToolCallsArray.Count - 1 do
  begin
    if ToolCallsArray.Items[i] is TJSONObject then
    begin
      ToolCallObj := TJSONObject(ToolCallsArray.Items[i]);
      Result[i].Id := ToolCallObj.Strings['id'];
      
      FunctionObj := ToolCallObj.Objects['function'];
      if FunctionObj <> nil then
      begin
        Result[i].FunctionName := FunctionObj.Strings['name'];
        Result[i].Arguments := FunctionObj.Strings['arguments'];
      end;
    end;
  end;
end;

procedure TOpenAIProvider.ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
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
    
    // Parse choices array
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

procedure TOpenAIProvider.ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
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
      
      // Use the stream-based Post method like the docs example
      LStreamClient.Post(FBaseUrl + '/chat/completions', LResponseStream);
      
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

end.
