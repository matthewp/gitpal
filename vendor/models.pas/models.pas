unit models;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser;

type
  { TLLMMessageRole }
  TLLMMessageRole = (lmrSystem, lmrUser, lmrAssistant, lmrTool);

  { TLLMStreamCallback - Callback for receiving streaming chunks }
  TLLMStreamCallback = procedure(const AChunk: string; AFinished: Boolean) of object;

  { TLLMMessage }
  TLLMMessage = record
    Role: TLLMMessageRole;
    Content: string;
    Name: string;
    ToolCallId: string;
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

  { ILLMProvider - Common interface for all LLM providers }
  ILLMProvider = interface
    ['{B8E5F4A2-1D2C-4E3F-8A9B-5C6D7E8F9A0B}']
    function ChatCompletion(const AModel: string; const AMessages: array of TLLMMessage; 
      ATemperature: Double = 0.7; AMaxTokens: Integer = 2048): TLLMChatCompletionResponse;
    procedure ChatCompletionStream(const AModel: string; const AMessages: array of TLLMMessage; 
      ACallback: TLLMStreamCallback; ATemperature: Double = 0.7; AMaxTokens: Integer = 2048);
    function GetProviderName: string;
    function GetDefaultModel: string;
  end;

  { TOpenAIProvider - OpenAI implementation }
  TOpenAIProvider = class(TInterfacedObject, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
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
  end;

  { TGeminiProvider - Google Gemini implementation }
  TGeminiProvider = class(TInterfacedObject, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamChunk(const ALine: string; ACallback: TLLMStreamCallback);
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
  end;

  { TClaudeProvider - Anthropic Claude implementation }
  TClaudeProvider = class(TInterfacedObject, ILLMProvider)
  private
    FHttpClient: TFPHttpClient;
    FApiKey: string;
    FBaseUrl: string;
    function MessageRoleToString(ARole: TLLMMessageRole): string;
    function StringToMessageRole(const AStr: string): TLLMMessageRole;
    function ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
    procedure ParseStreamEvent(const AEventType: string; const AData: string; ACallback: TLLMStreamCallback);
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
  FillChar(Result, SizeOf(Result), 0);
  SetLength(Result.Choices, 0);

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
            Result.Choices[LIndex].Message.Content := LMessageObj.Strings['content'];
            if LMessageObj.Find('name') <> nil then
              Result.Choices[LIndex].Message.Name := LMessageObj.Strings['name'];
            if LMessageObj.Find('tool_call_id') <> nil then
              Result.Choices[LIndex].Message.ToolCallId := LMessageObj.Strings['tool_call_id'];
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

{ TGeminiProvider }

function TGeminiProvider.MessageRoleToString(ARole: TLLMMessageRole): string;
begin
  case ARole of
    lmrSystem: Result := AnsiString('system');
    lmrUser: Result := AnsiString('user');
    lmrAssistant: Result := AnsiString('assistant');
    lmrTool: Result := AnsiString('tool');
    else Result := AnsiString('');
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

function TGeminiProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
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
  FillChar(Result, SizeOf(Result), 0);
  SetLength(Result.Choices, 0);

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

    // Parse successful response (OpenAI compatible format)
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
            Result.Choices[LIndex].Message.Content := LMessageObj.Strings['content'];
            if LMessageObj.Find('name') <> nil then
              Result.Choices[LIndex].Message.Name := LMessageObj.Strings['name'];
            if LMessageObj.Find('tool_call_id') <> nil then
              Result.Choices[LIndex].Message.ToolCallId := LMessageObj.Strings['tool_call_id'];
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

function TClaudeProvider.ParseChatCompletionResponse(const AJsonString: string): TLLMChatCompletionResponse;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LErrorData: TJSONData;
  LErrorObj: TJSONObject;
  LContentArray: TJSONArray;
  LContentItem: TJSONObject;
  LUsageObj: TJSONObject;
begin
  FillChar(Result, SizeOf(Result), 0);
  SetLength(Result.Choices, 0);

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
      
      if (LContentArray.Count > 0) and (LContentArray.Items[0] is TJSONObject) then
      begin
        LContentItem := TJSONObject(LContentArray.Items[0]);
        Result.Choices[0].Message.Role := lmrAssistant;
        if LContentItem.Find('text') <> nil then
          Result.Choices[0].Message.Content := LContentItem.Strings['text'];
      end;
      
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

end.