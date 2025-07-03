# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Important: Project Name Convention

**ALWAYS use "gitpal" (all lowercase, single word) when referring to this project.** Never use "GitPal", "Gitpal", "git-pal", or any other variant. The correct usage is:
- ✅ gitpal
- ❌ GitPal
- ❌ Gitpal  
- ❌ git-pal

## Project Overview

This is a CLI tool written in Pascal that reads git status and creates good commit messages using AI. The project uses two main vendored dependencies:
- `models.pas` - LLM abstraction library for connecting to various AI APIs (OpenAI, Claude, etc.)
- `bobaui` - Terminal User Interface (TUI) framework for rendering interactive components

## Architecture

### Core Dependencies
- **models.pas** (`vendor/models.pas/`): Provides `TLLMApiClient` for LLM communication with support for streaming, chat completions, and multiple providers
- **bobaui** (`vendor/bobaui/`): TUI framework with components like spinners, menus, and interactive elements for terminal applications

### Critical Import Order Requirement
**IMPORTANT**: `bobaui` must be the **first import** in the main program file (`src/gitpal.pas`).

Due to FreePascal unit initialization order dependencies, importing `bobaui` after system units like `cthreads` or `BaseUnix` causes access violations during program cleanup. Always structure imports as:

```pascal
uses
  bobaui,           // MUST be first
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  SysUtils,
  // other units...
```

This ensures proper initialization order and prevents cleanup conflicts between the TUI framework and system threading libraries.

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

### Forward Declarations
When functions call each other or need to be ordered differently than their natural dependency order, use forward declarations (similar to function prototypes in C):

```pascal
// Forward declarations after type definitions
procedure LogToFile(const LogMessage: string; const FileName: string = 'gitpal-debug.log'); forward;
function ExecuteGitCommit(const CommitMessage: string): Boolean; forward;

// Now functions can be implemented in any order and call each other
function TCommitModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
begin
  // Can call ExecuteGitCommit even though it's implemented later
  ExecuteGitCommit(FCommitMessage);
end;

// Actual implementations
procedure LogToFile(const LogMessage: string; const FileName: string);
begin
  // Implementation here
end;

function ExecuteGitCommit(const CommitMessage: string): Boolean;
begin
  // Can call LogToFile here
  LogToFile('Executing commit...');
end;
```

**Best practices:**
- Place forward declarations after type definitions but before any implementations
- Use forward declarations to solve circular dependencies and ordering issues
- The forward declaration must exactly match the implementation signature
- Particularly useful for utility functions that need to be called from multiple places

## Dependencies and Build Requirements

### Critical Warning: Never Modify Vendor Code
**NEVER** modify any files in the `vendor/` directory. Vendor dependencies should be treated as read-only external libraries. If there are issues with vendor code:
1. Report the issue to the vendor's repository
2. Ask the project maintainer for guidance
3. Consider using a different version of the dependency
4. Look for workarounds in your own code, not the vendor code

Modifying vendor code breaks dependency management and creates maintenance nightmares.

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

### Build System
**IMPORTANT**: Always use the CMake build system instead of calling `fpc` directly. 

To build the project:
```bash
# Configure the build (only needed once or when CMakeLists.txt changes)
cmake -B build

# Build the project
cmake --build build
```

The CMake configuration automatically handles:
- OpenSSL 1.1 linking flags for HTTPS support
- Vendor dependency paths (bobaui, models.pas)
- Source file paths and compilation order
- Platform-specific compiler flags

**Never use `fpc` directly** - this bypasses the dependency management and can cause linking issues.

## Development Notes

### Project Structure
- Main program file: `src/gitpal.pas` (CLI entry point with argument parsing)
- Command modules: `src/command_commit.pas`, `src/command_changelog.pas` (feature implementations)
- Shared git operations: `src/git.pas` (centralized git repository interaction)
- Vendor dependencies are in `vendor/` directory
- Follow vendor library patterns for LLM integration and TUI components

### Git Operations Architecture
The project uses a centralized git operations system through `src/git.pas`:

- **`TGitResult`** record: Standardized result type with `Success`, `Output`, `ErrorMessage`, and `ExitCode` fields
- **`TGitRepository`** class: Static methods for all git operations with built-in security and error handling
- **Security features**: Input sanitization, argument validation, and command injection prevention
- **Consistent error handling**: All git operations return `TGitResult` for uniform error processing
- **Support for git worktrees**: Repository detection works correctly with both regular repos and worktrees

Key methods:
- `TGitRepository.IsRepository(Path)` - Detect git repositories (including worktrees)
- `TGitRepository.Commit(Message)` - Execute git commits with sanitized messages
- `TGitRepository.Add(Files)` - Stage files for commit
- `TGitRepository.GetDiff(Options)` - Get git diffs with custom options
- `TGitRepository.GetTags(SortOptions)` - Retrieve git tags with sorting
- `TGitRepository.GetCommitRange(FromRef, ToRef)` - Get commits between references

**Always use git.pas methods instead of calling git commands directly** - this ensures security, consistency, and proper error handling across the application.

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
# Update CMakeLists.txt to point to new version path
# Remove old version directory if desired
```

### Updating models.pas
models.pas releases are available on SourceHut. Versions are installed in versioned directories under `vendor/models.pas/v{VERSION}/`.

Download URL pattern: `https://git.sr.ht/~matthewp/models.pas/refs/download/v{VERSION}/models.pas-{VERSION}.zip`

Update process:
```bash
# Replace {VERSION} with the target version (e.g., 0.0.1)
mkdir -p vendor/models.pas/v{VERSION}
cd vendor/models.pas/v{VERSION} && curl -L "https://git.sr.ht/~matthewp/models.pas/refs/download/v{VERSION}/models.pas-{VERSION}.zip" -o models.pas.zip && unzip -q models.pas.zip && rm models.pas.zip
# Update CMakeLists.txt to point to new version path
# Remove old version directory if desired
```

Both dependency update processes will:
1. Create a new versioned directory for the release
2. Download and extract the specified version 
3. Remove the temporary zip file
4. Requires updating CMakeLists.txt to use new version path
5. Optionally remove old version directories


## Release Process

To create a new release of gitpal:

1. **Update version number** in `src/gitpal.pas`:
   ```pascal
   const
     AppVersion = '0.0.6';  // Update this when tagging new releases
   ```

2. **Create and push git tag**:
   ```bash
   git add src/gitpal.pas
   git commit -m "feat: Bump version to 0.0.6"
   git tag v0.0.6
   git push origin main
   git push origin v0.0.6
   ```

3. **CI/CD automation**: The `.build.yml` GitHub Actions workflow will automatically:
   - Build binaries for macOS and Linux
   - Create a GitHub release
   - Upload binaries as release assets
   - Update Homebrew formula (if configured)

