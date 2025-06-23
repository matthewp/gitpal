# FreePascal LLM API Client

A unified interface for interacting with various Large Language Model (LLM) APIs in FreePascal, similar to Python libraries like LangChain.

## Features

- **Multi-Provider Support**: OpenAI GPT, Anthropic Claude, Google Gemini
- **Tool/Function Calling**: Complete implementation for all providers
- **Streaming Responses**: Real-time response streaming for better UX
- **Type Safety**: Strong typing with proper error handling
- **SSL/HTTPS Support**: Secure connections using OpenSSL
- **Clean Architecture**: Modular design with provider-specific implementations

## Supported Providers

### OpenAI GPT
- **Provider Class**: `TOpenAIProvider`
- **Unit**: `openai_provider`
- **Default Model**: `gpt-3.5-turbo`
- **Supported Models**: `gpt-3.5-turbo`, `gpt-4`, `gpt-4-turbo`, `gpt-4o`
- **Environment Variable**: `OPENAI_API_KEY`
- **Features**: Chat completion, streaming, tool calling
- **API Documentation**: https://platform.openai.com/docs/api-reference

### Anthropic Claude
- **Provider Class**: `TClaudeProvider`
- **Unit**: `claude_provider`
- **Default Model**: `claude-3-haiku-20240307`
- **Supported Models**: `claude-3-haiku-20240307`, `claude-3-sonnet-20240229`, `claude-3-opus-20240229`, `claude-3-5-sonnet-20241022`
- **Environment Variable**: `ANTHROPIC_API_KEY`
- **Features**: Chat completion, streaming, tool calling, system message separation
- **API Documentation**: https://docs.anthropic.com/claude/reference

### Google Gemini
- **Provider Class**: `TGeminiProvider`
- **Unit**: `gemini_provider`
- **Default Model**: `gemini-1.5-flash`
- **Supported Models**: `gemini-1.5-flash`, `gemini-1.5-pro`, `gemini-2.5-flash`
- **Environment Variable**: `GEMINI_API_KEY`
- **Features**: Chat completion, streaming, tool calling with native Gemini format
- **API Documentation**: https://ai.google.dev/gemini-api/docs

## Quick Start

### Prerequisites

- FreePascal 3.2.2 or later
- OpenSSL 1.1.x (required for HTTPS support)

**Important**: FreePascal 3.2.2 requires OpenSSL 1.1.x series. OpenSSL 3.0+ is not compatible until FPC 3.2.4.

### Installation

#### Install OpenSSL 1.1 (macOS)
```bash
brew install openssl@1.1
```

#### Build the Project
```bash
# Build all examples
make

# Build specific examples
make chat
make tools-openai
make tools-claude  
make tools-gemini
```

### Configuration

Set your API keys via environment variables:

```bash
export OPENAI_API_KEY="your-openai-api-key"
export ANTHROPIC_API_KEY="your-anthropic-api-key"  
export GEMINI_API_KEY="your-gemini-api-key"
```

### Basic Usage

#### Interactive Chat Client
```bash
# Use default provider (OpenAI)
./bin/chat chat "Hello, how are you?"

# Specify provider and model
./bin/chat chat "Tell me a joke" --provider claude --model claude-3-sonnet-20240229
./bin/chat chat "Explain AI" --provider gemini --model gemini-1.5-pro
```

#### Using Makefile Run Targets
```bash
# Run chat with custom message
make run-chat ARGS="chat 'Hello world' --provider openai"

# Test tool calling examples
make run-tools-openai
make run-tools-claude
make run-tools-gemini
```

## Programming Interface

### Basic Chat Completion

#### OpenAI Example
```pascal
uses models, openai_provider;

var
  Provider: TOpenAIProvider;
  Messages: array[0..0] of TLLMMessage;
  Response: TLLMChatCompletionResponse;
begin
  Provider := TOpenAIProvider.Create(TOpenAIProvider.GetApiKeyFromEnvironment);
  try
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Hello, world!';
    
    Response := Provider.ChatCompletion('gpt-3.5-turbo', Messages);
    WriteLn(Response.Choices[0].Message.Content);
  finally
    Provider.Free;
  end;
end;
```

#### Claude Example
```pascal
uses models, claude_provider;

var
  Provider: TClaudeProvider;
  Messages: array[0..0] of TLLMMessage;
  Response: TLLMChatCompletionResponse;
begin
  Provider := TClaudeProvider.Create(TClaudeProvider.GetApiKeyFromEnvironment);
  try
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Hello, world!';
    
    Response := Provider.ChatCompletion('claude-3-haiku-20240307', Messages);
    WriteLn(Response.Choices[0].Message.Content);
  finally
    Provider.Free;
  end;
end;
```

#### Gemini Example
```pascal
uses models, gemini_provider;

var
  Provider: TGeminiProvider;
  Messages: array[0..0] of TLLMMessage;
  Response: TLLMChatCompletionResponse;
begin
  Provider := TGeminiProvider.Create(TGeminiProvider.GetApiKeyFromEnvironment);
  try
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Hello, world!';
    
    Response := Provider.ChatCompletion('gemini-1.5-flash', Messages);
    WriteLn(Response.Choices[0].Message.Content);
  finally
    Provider.Free;
  end;
end;
```

### Streaming Responses

```pascal
procedure OnStreamChunk(const AChunk: string; AFinished: Boolean);
begin
  if AFinished then
    WriteLn('')
  else
    Write(AChunk);
end;

var
  Provider: TOpenAIProvider;
  Messages: array[0..0] of TLLMMessage;
begin
  Provider := TOpenAIProvider.Create('your-api-key');
  try
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Write a short story';
    
    Provider.ChatCompletionStream('gpt-3.5-turbo', Messages, @OnStreamChunk);
  finally
    Provider.Free;
  end;
end;
```

### Tool/Function Calling

#### Creating Custom Tools

Here's a complete example showing how to create your own tools:

```pascal
uses models, openai_provider, fpjson, jsonparser, DateUtils, SysUtils;

type
  { Custom tool context with multiple tools }
  TMyToolContext = class(TBaseToolContext)
  private
    function GetCurrentTime: string;
    function GetWeather(const City: string): string;
    function CalculateSum(A, B: Integer): Integer;
  public
    function ExecuteTool(const ToolCall: TToolCall): TToolResult; override;
    function GetAvailableTools: TToolFunctionArray; override;
  end;

{ TMyToolContext implementation }

function TMyToolContext.GetCurrentTime: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' UTC';
end;

function TMyToolContext.GetWeather(const City: string): string;
begin
  // Mock weather data - in real implementation, call weather API
  Result := Format('Weather in %s: Sunny, 22°C', [City]);
end;

function TMyToolContext.CalculateSum(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TMyToolContext.ExecuteTool(const ToolCall: TToolCall): TToolResult;
var
  LParser: TJSONParser;
  LData: TJSONData;
  LObj: TJSONObject;
  LCity: string;
  LA, LB: Integer;
begin
  Result.ToolCallId := ToolCall.Id;
  Result.IsError := False;
  
  try
    if ToolCall.FunctionName = 'get_current_time' then
    begin
      Result.Content := GetCurrentTime;
    end
    else if ToolCall.FunctionName = 'get_weather' then
    begin
      // Parse arguments JSON
      LParser := TJSONParser.Create(ToolCall.Arguments, DefaultOptions);
      try
        LData := LParser.Parse;
        try
          if Assigned(LData) and (LData is TJSONObject) then
          begin
            LObj := TJSONObject(LData);
            LCity := LObj.Get('city', '');
            Result.Content := GetWeather(LCity);
          end
          else
            raise Exception.Create('Invalid arguments format');
        finally
          LData.Free;
        end;
      finally
        LParser.Free;
      end;
    end
    else if ToolCall.FunctionName = 'calculate_sum' then
    begin
      // Parse arguments JSON
      LParser := TJSONParser.Create(ToolCall.Arguments, DefaultOptions);
      try
        LData := LParser.Parse;
        try
          if Assigned(LData) and (LData is TJSONObject) then
          begin
            LObj := TJSONObject(LData);
            LA := LObj.Get('a', 0);
            LB := LObj.Get('b', 0);
            Result.Content := IntToStr(CalculateSum(LA, LB));
          end
          else
            raise Exception.Create('Invalid arguments format');
        finally
          LData.Free;
        end;
      finally
        LParser.Free;
      end;
    end
    else
    begin
      Result.Content := 'Unknown tool: ' + ToolCall.FunctionName;
      Result.IsError := True;
    end;
  except
    on E: Exception do
    begin
      Result.Content := 'Tool execution error: ' + E.Message;
      Result.IsError := True;
    end;
  end;
end;

function TMyToolContext.GetAvailableTools: TToolFunctionArray;
begin
  SetLength(Result, 3);
  
  // Tool 1: Get current time (no parameters)
  Result[0].Name := 'get_current_time';
  Result[0].Description := 'Get the current date and time in UTC';
  SetLength(Result[0].Parameters, 0); // No parameters
  
  // Tool 2: Get weather (one required parameter)
  Result[1].Name := 'get_weather';
  Result[1].Description := 'Get the current weather for a city';
  SetLength(Result[1].Parameters, 1);
  Result[1].Parameters[0].Name := 'city';
  Result[1].Parameters[0].ParamType := tptString;
  Result[1].Parameters[0].Description := 'The name of the city';
  Result[1].Parameters[0].Required := True;
  
  // Tool 3: Calculate sum (two required parameters)
  Result[2].Name := 'calculate_sum';
  Result[2].Description := 'Calculate the sum of two integers';
  SetLength(Result[2].Parameters, 2);
  Result[2].Parameters[0].Name := 'a';
  Result[2].Parameters[0].ParamType := tptInteger;
  Result[2].Parameters[0].Description := 'First integer';
  Result[2].Parameters[0].Required := True;
  Result[2].Parameters[1].Name := 'b';
  Result[2].Parameters[1].ParamType := tptInteger;
  Result[2].Parameters[1].Description := 'Second integer';
  Result[2].Parameters[1].Required := True;
end;

{ Usage example }
var
  Provider: TOpenAIProvider;
  ToolContext: IToolContext;
  Messages: array[0..0] of TLLMMessage;
  Tools: TToolFunctionArray;
  Response: TLLMChatCompletionResponse;
begin
  Provider := TOpenAIProvider.Create(TOpenAIProvider.GetApiKeyFromEnvironment);
  try
    ToolContext := TMyToolContext.Create;  // Interface reference - no manual cleanup needed
    
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'What time is it and what''s the weather in Paris?';
    
    Tools := ToolContext.GetAvailableTools;
    Response := Provider.ChatCompletionWithTools(
      'gpt-3.5-turbo', Messages, Tools, ToolContext);
    
    WriteLn(Response.Choices[0].Message.Content);
  finally
    Provider.Free;  // Only free the provider, not the tool context
  end;
end;
```

#### Available Parameter Types

When defining tool parameters, you can use these types:

- `tptString` - Text values
- `tptInteger` - Whole numbers  
- `tptNumber` - Decimal numbers
- `tptBoolean` - True/false values
- `tptArray` - Lists of values
- `tptObject` - Complex nested objects

#### Provider-Specific Tool Calling

All providers support the same tool calling interface, but you can use provider-specific implementations:

```pascal
// Works with OpenAI
Provider := TOpenAIProvider.Create(TOpenAIProvider.GetApiKeyFromEnvironment);

// Works with Claude  
Provider := TClaudeProvider.Create(TClaudeProvider.GetApiKeyFromEnvironment);

// Works with Gemini
Provider := TGeminiProvider.Create(TGeminiProvider.GetApiKeyFromEnvironment);

// Same tool calling code works with all providers
Response := Provider.ChatCompletionWithTools(Model, Messages, Tools, ToolContext);
```

## Architecture

### Core Components

- **models.pas**: Base types, interfaces, and common functionality
- **openai_provider.pas**: OpenAI GPT implementation with tool calling
- **claude_provider.pas**: Anthropic Claude implementation with tool calling
- **gemini_provider.pas**: Google Gemini implementation with tool calling
- **basetoolprovider.pas**: Base tool provider with shared functionality
- **toolcontext.pas**: Tool context utilities and interfaces

### Key Interfaces

- **ILLMProvider**: Common interface for all LLM providers
- **IToolContext**: Interface for tool/function calling implementations
- **TBaseToolProvider**: Base class with shared tool calling logic

### Data Structures

- **TLLMMessage**: Unified message format across providers
- **TToolFunction**: Tool/function definition for AI function calling
- **TLLMChatCompletionResponse**: Standardized response format

## Examples

The `examples/` directory contains working examples:

- **chat.pp**: Interactive chat client supporting all providers
- **tools_openai.pp**: OpenAI function calling demonstration
- **tools_claude.pp**: Claude tool calling demonstration  
- **tools_gemini.pp**: Gemini function calling demonstration

## Build System

The project uses a comprehensive Makefile with the following targets:

### Build Targets
- `make all` - Build all examples (default)
- `make chat` - Build interactive chat client
- `make tools-openai` - Build OpenAI tools example
- `make tools-claude` - Build Claude tools example
- `make tools-gemini` - Build Gemini tools example

### Run Targets
- `make run-chat ARGS="..."` - Run chat client with arguments
- `make run-tools-*` - Run specific tool calling examples

### Utility Targets
- `make clean` - Remove build artifacts
- `make config` - Show build configuration
- `make help` - Show all available targets

## Development Guidelines

### FreePascal Best Practices

Always include these compiler directives:
```pascal
{$mode objfpc}     // Enable Object Pascal mode
{$codepage UTF8}   // UTF-8 string encoding support  
{$H+}              // Enable long strings (AnsiString)
```

### Naming Conventions
- Classes: PascalCase with 'T' prefix (`TOpenAIProvider`)
- Private fields: PascalCase with 'F' prefix (`FApiKey`)
- Parameters: PascalCase with 'A' prefix (`AModel`)
- Methods/Properties: PascalCase (`ChatCompletion`)

### Memory Management

#### Standard Object Memory Management
For standard classes, always use try/finally blocks for object cleanup:
```pascal
MyObject := TMyClass.Create;
try
  // Use object
finally
  MyObject.Free;
end;
```

#### Tool Context Memory Management
**IMPORTANT**: `TBaseToolContext` and its subclasses inherit from `TInterfacedObject` and use automatic reference counting. **Never call `.Free()` manually** on these objects.

**❌ WRONG - Causes Access Violations:**
```pascal
var
  ToolContext: TMyToolContext;
begin
  ToolContext := TMyToolContext.Create;
  try
    // Use the tool context...
  finally
    ToolContext.Free;  // ❌ This will cause an access violation!
  end;
end;
```

**✅ CORRECT - Use Interface References:**
```pascal
var
  ToolContext: IToolContext;
begin
  ToolContext := TMyToolContext.Create;
  // No try/finally needed - automatic cleanup when interface goes out of scope
  // Use the tool context...
end;
```

**Why This Matters:**
- `TBaseToolContext` inherits from `TInterfacedObject` for automatic memory management
- Interface reference counting handles object lifetime automatically
- Manual `.Free()` calls can cause double-free scenarios and access violations
- This pattern is consistent across all LLM providers in the library

## Contributing

1. Follow the existing code style and naming conventions
2. Add comprehensive error handling for API interactions
3. Include proper memory management with try/finally blocks
4. Test with all supported providers when adding new features
5. Update documentation for new functionality

## License

This project is licensed under the BSD 3-Clause License - see the [LICENSE](LICENSE) file for details.

## Troubleshooting

### Common Issues

**SSL/TLS Errors**
- Ensure OpenSSL 1.1.x is installed (not 3.0+)
- Check that library paths are correctly configured

**API Key Errors**
- Verify environment variables are set correctly
- Ensure API keys have necessary permissions

**Build Errors**
- Check FreePascal version (3.2.2+ required)
- Verify OpenSSL development headers are installed

### Getting Help

For issues and questions:
1. Check the examples in the `examples/` directory
2. Review the CLAUDE.md file for detailed development guidelines
3. Create an issue on the project repository

## Roadmap

- [x] Complete Claude and Gemini provider extraction
- [ ] Add support for additional LLM providers (Cohere, Mistral, etc.)
- [ ] Implement async/non-blocking operations
- [ ] Add comprehensive unit tests with FPCUnit
- [ ] Create detailed API documentation
- [ ] Add support for image/multimodal inputs
- [ ] Implement conversation memory management
- [ ] Add configuration file support
- [ ] Implement request/response logging and debugging
- [ ] Add retry mechanisms with exponential backoff