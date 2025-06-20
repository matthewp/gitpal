# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a CLI tool written in Pascal that reads git status and creates good commit messages using AI. The project uses two main vendored dependencies:
- `models.pas` - LLM abstraction library for connecting to various AI APIs (OpenAI, Claude, etc.)
- `bobaui` - Terminal User Interface (TUI) framework for rendering interactive components

## Architecture

### Core Dependencies
- **models.pas** (`vendor/models.pas/`): Provides `TLLMApiClient` for LLM communication with support for streaming, chat completions, and multiple providers
- **bobaui** (`vendor/bobaui/`): TUI framework with components like spinners, menus, and interactive elements for terminal applications

### Key Components
- `bobaui.pas`: Core TUI framework with message handling, keyboard input, and terminal control
- `bobacomponents.pas`: UI components (spinners, menus, dropdowns) for interactive CLI experiences  
- `bobastyle.pas`: Styling and theming for TUI components
- `models.pas`: LLM client with JSON parsing, HTTP requests, and streaming support

## FreePascal Development Guidelines

### Essential Compiler Directives
Always include at the start of units:
```pascal
{$mode objfpc}     // Enable Object Pascal mode
{$codepage UTF8}   // UTF-8 string encoding support  
{$H+}              // Enable long strings (AnsiString)
```

### Critical String Pattern
Prevent UTF-8 corruption by using explicit AnsiString casting:
```pascal
// CORRECT
Result := AnsiString('');
SomeVar := AnsiString('');

// WRONG - can corrupt UTF-8
Result := '';
SomeVar := '';
```

### Naming Conventions
- Classes: PascalCase with 'T' prefix (`TLLMApiClient`, `TSpinnerType`)
- Private fields: PascalCase with 'F' prefix (`FHttpClient`, `FApiKey`)
- Parameters: PascalCase with 'A' prefix (`AApiKey`, `AMessages`)
- Methods/Properties: PascalCase (`ChatCompletion`, `ParseResponse`)

### Memory Management
Always use try/finally blocks for object cleanup:
```pascal
MyObject := TMyClass.Create;
try
  // Use object
finally
  MyObject.Free;
end;
```

## Dependencies and Build Requirements

### OpenSSL Version Requirements
**Critical**: FreePascal 3.2.2 requires OpenSSL 1.1.x series. OpenSSL 3.0+ is not compatible until FPC 3.2.4.

Install OpenSSL 1.1 via Homebrew:
```bash
brew install openssl@1.1
```

### Required Pascal Units
- `fphttpclient` - HTTP requests for LLM APIs
- `fpjson`, `jsonparser` - JSON parsing for API responses
- `opensslsockets` - HTTPS support
- Platform-specific units: `BaseUnix`, `TermIO`, `Unix` (for terminal control)

### Compilation
Use proper OpenSSL 1.1 flags for HTTPS support:
```bash
fpc -Fl/opt/homebrew/opt/openssl@1.1/lib -k-rpath -k/opt/homebrew/opt/openssl@1.1/lib -k-lssl -k-lcrypto program.pp
```

## Development Notes

### Project Structure
- Main source files should be in project root (currently minimal setup)
- Vendor dependencies are in `vendor/` directory
- Follow vendor library patterns for LLM integration and TUI components

### LLM Integration Patterns
- Use `TLLMMessage` records for chat messages with roles (system, user, assistant)
- Implement streaming callbacks with `TLLMStreamCallback` for real-time responses
- Handle API responses with proper error checking using `TLLMChatCompletionResponse`

### TUI Development
- Use `bobaui` message-driven architecture with `TMsg` base class
- Implement components using `TSpinnerType` for loading states
- Handle keyboard input through message system for interactive CLI

### API Key Management
Set LLM API keys via environment variables:
```bash
export OPENAI_API_KEY="your-api-key-here"
export ANTHROPIC_API_KEY="your-api-key-here"
```

## Dependency Management

### Updating BobaUI
BobaUI releases are available on SourceHut. Versions are installed in versioned directories under `vendor/bobaui/v{VERSION}/`.

Download URL pattern: `https://git.sr.ht/~matthewp/bobaui/refs/download/v{VERSION}/bobaui-{VERSION}.zip`

Update process:
```bash
# Replace {VERSION} with the target version (e.g., 0.1.6)
mkdir -p vendor/bobaui/v{VERSION}
cd vendor/bobaui/v{VERSION} && curl -L "https://git.sr.ht/~matthewp/bobaui/refs/download/v{VERSION}/bobaui-{VERSION}.zip" -o bobaui.zip && unzip -q bobaui.zip && rm bobaui.zip
# Update Makefile to point to new version path
# Remove old version directory if desired
```

This process will:
1. Create a new versioned directory for the release
2. Download and extract the specified version 
3. Remove the temporary zip file
4. Requires updating Makefile to use new version path
5. Optionally remove old version directories

**Current version**: v0.1.6 (located in `vendor/bobaui/v0.1.6/`)