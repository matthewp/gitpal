# GitPal Changelog Feature Implementation Plan

## Overview

This document outlines the implementation plan for GitPal's changelog feature. The goal is to implement `gitpal changelog` which will automatically update CHANGELOG.md by analyzing git tags and commits using AI.

## Current State Analysis

### Existing Implementation
- **Current Status:** Stub implementation in `src/app.pas:1269-1272`
- **Help Text:** Available in `ShowChangelogHelp()` (lines 951-964) 
- **Command Parsing:** Integrated in main argument parser (lines 1348-1349)

### Repository Analysis
- **Git Tags Available:** v0.0.7, v0.0.6, v0.0.5, v0.0.4, v0.0.3, v0.0.2, 0.0.1
- **CHANGELOG.md:** Not currently present in repository
- **Recent Commits:** Mix of features, fixes, and version bumps

### Models.pas Capabilities
- **Tool Calling:** Not currently supported - only has basic `ChatCompletion` interface
- **Providers:** OpenAI, Gemini, Claude (TOpenAIProvider, TGeminiProvider, TClaudeProvider)
- **Streaming:** Available but not needed for changelog generation
- **JSON Support:** Full JSON parsing available via fpjson

## Implementation Strategy

### ⏳ WAITING: Tool Calling Support in models.pas

**Current Status**: Implementation is blocked pending tool calling support in models.pas. See `tool_calling.md` for detailed specifications.

### Approach: Single AI Conversation with Tool Calling

Once tool calling is implemented in models.pas, the changelog feature will use a single AI conversation where the AI can directly call tools to gather git data and update the changelog.

**Planned Flow**:
1. User runs `gitpal changelog`
2. AI uses tools to gather all necessary data:
   - `get_git_tags()` - Get current repository tags
   - `read_changelog()` - Read existing CHANGELOG.md
   - `get_commits_between(from_tag, to_tag)` - Get commit history for missing tags
3. AI generates complete changelog entries with full context
4. AI uses `write_changelog(content)` to update the file
5. Single conversation completes the entire process

**Benefits over multi-step approach**:
- No error-prone string parsing
- Context preserved throughout conversation
- AI can handle failures gracefully and retry
- More reliable and robust implementation

## Updated Implementation Plan (Post Tool Calling)

### 1. Tool Context Implementation

The main implementation will be a `TGitPalToolContext` class that provides these tools to the AI:

```pascal
type
  TGitPalToolContext = class(TInterfacedObject, IToolContext)
  public
    function ExecuteTool(const ToolCall: TToolCall): TToolResult;
    function GetAvailableTools: array of TToolFunction;
  end;
```

**Available Tools:**
- `get_git_tags()` - Returns sorted list of git tags
- `get_commits_between(from_tag, to_tag)` - Returns commit history between tags
- `read_changelog()` - Returns current CHANGELOG.md content or "File not found"
- `write_changelog(content)` - Writes complete changelog content to file

### 2. Simplified Implementation Flow

```pascal
procedure RunChangelogCommand;
var
  Provider: ILLMProvider;
  Messages: array of TLLMMessage;
  Tools: array of TToolFunction;
  ToolContext: TGitPalToolContext;
  Response: TLLMChatCompletionResponse;
  ApiKey: string;
begin
  // Setup provider with tool calling support
  ApiKey := TGeminiProvider.GetApiKeyFromEnvironment;
  Provider := TGeminiProvider.Create(ApiKey);
  
  if not Provider.SupportsToolCalling then
  begin
    writeln('Error: Selected provider does not support tool calling');
    Exit;
  end;
  
  // Setup tool context and conversation
  ToolContext := TGitPalToolContext.Create;
  try
    SetLength(Messages, 1);
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Please update my CHANGELOG.md file by analyzing my git tags ' +
                          'and adding entries for any missing versions. Use the available ' +
                          'tools to get git information and read/write the changelog file.';
    
    Tools := ToolContext.GetAvailableTools;
    
    // Single tool-calling conversation handles everything
    Response := Provider.ChatCompletionWithTools(
      Provider.GetDefaultModel,
      Messages,
      Tools,
      ToolContext,
      0.3,
      2000
    );
    
    if Length(Response.Choices) > 0 then
      writeln(Response.Choices[0].Message.Content)
    else
      writeln('No response from AI');
      
  finally
    ToolContext.Free;
  end;
end;
```

### 3. Tool Implementation Details

Each tool will be implemented as a method in `TGitPalToolContext`:

```pascal
function GetGitTags: string;          // Uses `git tag --list --sort=-version:refname`
function GetCommitsBetween: string;   // Uses `git log --oneline from_tag..to_tag`
function ReadChangelogFile: string;   // Reads CHANGELOG.md or returns error
function WriteChangelogFile: Boolean; // Writes content with error handling
```

### 4. File Structure Changes

#### New Code to Add to `src/app.pas`
1. **Tool Context Class** (after existing type definitions):
   - `TGitPalToolContext` class with tool execution logic

2. **Tool Implementation Methods**:
   - Git operations using existing process execution patterns
   - File I/O for changelog reading/writing
   - Error handling and validation

3. **Replace Stub** (line 1269-1272):
   - Replace `RunChangelogCommand` with tool-calling implementation

### 6. Error Handling

```pascal
// Handle cases like:
- No git repository
- No git tags found  
- AI API failures
- File permission issues
- Invalid changelog format
- Network connectivity issues
```

### 7. Testing Strategy

1. **Manual Testing Cases:**
   - Repository with no CHANGELOG.md
   - Repository with existing CHANGELOG.md (various formats)
   - Repository with no tags
   - Repository with all tags already in changelog
   - Mixed scenarios

2. **Edge Cases:**
   - Tags with no commits between them
   - Tags with identical commit content
   - Very large commit histories
   - Special characters in commit messages

## Implementation Priority (Updated)

1. **BLOCKED:** Wait for tool calling support in models.pas
2. **Phase 1:** Implement `TGitPalToolContext` with git operations
3. **Phase 2:** Implement tool-calling conversation flow
4. **Phase 3:** Error handling and edge cases  
5. **Phase 4:** Testing and refinement

## Estimated Complexity (Revised)

- **Tool Context Implementation:** Low-Medium (~100 lines)
- **AI Integration:** Low (single conversation with tool calling)
- **Overall:** Reduced complexity with tool calling, ~150-200 lines of new code

## Dependencies

- **REQUIRED:** Tool calling support in models.pas (see `tool_calling.md`)
- Uses existing: `models`, `Process`, `Classes`, `SysUtils`
- No additional dependencies

## Future Enhancements

1. **Format Detection:** Auto-detect changelog format (Keep-a-Changelog, custom, etc.)
2. **Interactive Mode:** Let user review/edit entries before writing  
3. **Rollback Support:** Undo changelog updates
4. **Template Support:** Custom changelog entry templates
5. **Advanced Tools:** Additional tools like:
   - `analyze_code_changes()` - Understand what changed beyond commit messages
   - `suggest_version_bump()` - Recommend semantic version changes
   - `validate_changelog_format()` - Check changelog follows standards