# Git Tool Calling Implementation Guide

## Overview

This guide provides comprehensive instructions for implementing git tool calling functionality using the `models.pas` FreePascal LLM abstraction library. Tool calling enables AI models to interact directly with git repositories through structured function calls, eliminating the need for error-prone string parsing of AI responses.

## Table of Contents

1. [Tool Calling Architecture](#tool-calling-architecture)
2. [Core Components](#core-components)
3. [Git Tool Implementation](#git-tool-implementation)
4. [Security Considerations](#security-considerations)
5. [Error Handling](#error-handling)
6. [Usage Examples](#usage-examples)
7. [Testing Strategies](#testing-strategies)

## Tool Calling Architecture

The `models.pas` library implements tool calling through several key components:

### Data Structures

```pascal
{ Tool parameter types }
TToolParameterType = (tptString, tptInteger, tptNumber, tptBoolean, tptArray, tptObject);

{ Tool parameter definition }
TToolParameter = record
  Name: string;
  ParamType: TToolParameterType;
  Description: string;
  Required: Boolean;
  EnumValues: array of string; // For restricted string values
end;

{ Tool function definition }
TToolFunction = record
  Name: string;
  Description: string;
  Parameters: array of TToolParameter;
end;

{ Tool call from AI }
TToolCall = record
  Id: string;          // Unique identifier for this call
  FunctionName: string;
  Arguments: string;   // JSON string of arguments
end;

{ Tool call result }
TToolResult = record
  ToolCallId: string;  // Matches TToolCall.Id
  Content: string;     // Result content or error message
  IsError: Boolean;    // True if tool execution failed
end;
```

### Tool Context Interface

```pascal
IToolContext = interface
  ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
  function ExecuteTool(const ToolCall: TToolCall): TToolResult;
  function GetAvailableTools: TToolFunctionArray;
end;
```

### Conversation Flow

1. **Tool Definition**: Define available tools via `GetAvailableTools`
2. **AI Request**: Send user message + tool definitions to LLM
3. **Tool Calls**: AI responds with structured tool calls
4. **Tool Execution**: Execute tools via `ExecuteTool`
5. **Result Integration**: Add tool results back to conversation
6. **Final Response**: AI provides final answer using tool data

## Core Components

### Base Tool Context Class

All tool implementations should inherit from `TBaseToolContext`:

```pascal
TBaseToolContext = class(TInterfacedObject, IToolContext)
protected
  function CreateToolResult(const AToolCallId: string; const AContent: string; AIsError: Boolean = False): TToolResult;
  function SafeParseToolArguments(const JsonString: string; out JsonObj: TJSONObject): Boolean;
public
  function ExecuteTool(const ToolCall: TToolCall): TToolResult; virtual; abstract;
  function GetAvailableTools: TToolFunctionArray; virtual; abstract;
end;
```

### Helper Methods

- **`CreateToolResult`**: Safely creates tool result records
- **`SafeParseToolArguments`**: Parses JSON arguments with proper error handling
- Automatic memory management for JSON objects

### Provider Integration

Use with any supported LLM provider:

```pascal
Response := Provider.ChatCompletionWithTools(
  'gpt-3.5-turbo',     // Model
  Messages,            // Conversation history
  Tools,               // Available tool definitions
  ToolContext,         // Tool execution context
  0.3,                 // Temperature
  2000                 // Max tokens
);
```

## Git Tool Implementation

### Recommended Git Tool Set

For comprehensive git operations, implement these core tools:

#### 1. Repository Information Tools

```pascal
// Get current repository status
get_git_status()
// Returns: Modified files, staged changes, branch info

// List all branches
get_git_branches(include_remote: Boolean = false)
// Returns: Array of branch names

// Get current branch
get_current_branch()
// Returns: Current branch name

// Get repository root
get_repo_root()
// Returns: Absolute path to repository root
```

#### 2. Tag and Version Tools

```pascal
// Get all tags sorted by version
get_git_tags(pattern: String = "", sort_by: String = "version")
// Parameters:
//   - pattern: Optional filter pattern (e.g., "v*")
//   - sort_by: "version", "date", "name"
// Returns: Array of tag names

// Get tag information
get_tag_info(tag_name: String)
// Returns: Tag date, message, commit hash

// Create new tag
create_tag(tag_name: String, message: String = "", commit: String = "HEAD")
// Returns: Success/failure status
```

#### 3. Commit History Tools

```pascal
// Get commits between references
get_commits_between(from_ref: String, to_ref: String, format: String = "standard")
// Parameters:
//   - from_ref: Starting reference (tag, branch, commit)
//   - to_ref: Ending reference (exclusive)
//   - format: "standard", "oneline", "detailed"
// Returns: Array of commit information

// Get commit details
get_commit_info(commit_hash: String)
// Returns: Author, date, message, files changed

// Get recent commits
get_recent_commits(count: Integer = 10, branch: String = "")
// Returns: Last N commits from specified branch
```

#### 4. File Operation Tools

```pascal
// Read file content at specific commit
read_file_at_commit(file_path: String, commit: String = "HEAD")
// Returns: File content as string

// Write file content
write_file(file_path: String, content: String)
// Returns: Success status

// Get file diff
get_file_diff(file_path: String, from_commit: String = "", to_commit: String = "HEAD")
// Returns: Diff content
```

#### 5. Changelog Tools

```pascal
// Read changelog file
read_changelog(file_path: String = "CHANGELOG.md")
// Returns: Current changelog content

// Write changelog file
write_changelog(content: String, file_path: String = "CHANGELOG.md")
// Returns: Success status

// Generate changelog entry
generate_changelog_entry(from_tag: String, to_tag: String, template: String = "standard")
// Returns: Formatted changelog entry
```

### Implementation Example

```pascal
unit git_tools;

{$mode objfpc}
{$codepage UTF8}
{$H+}

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser, models;

type
  TGitToolContext = class(TBaseToolContext)
  private
    FRepoPath: string;
    function ExecuteGitCommand(const Args: array of string): string;
    function SanitizeGitArgument(const Arg: string): string;
    function ValidateGitReference(const Ref: string): Boolean;
    
    // Tool implementations
    function GetGitTags(const Pattern: string = ''; const SortBy: string = 'version'): string;
    function GetCommitsBetween(const FromRef, ToRef: string; const Format: string = 'standard'): string;
    function ReadChangelogFile(const FilePath: string = 'CHANGELOG.md'): string;
    function WriteChangelogFile(const Content, FilePath: string): Boolean;
    function GetGitStatus: string;
    function GetCurrentBranch: string;
    
  public
    constructor Create(const ARepoPath: string = '.');
    function ExecuteTool(const ToolCall: TToolCall): TToolResult; override;
    function GetAvailableTools: TToolFunctionArray; override;
  end;

implementation

constructor TGitToolContext.Create(const ARepoPath: string);
begin
  inherited Create;
  FRepoPath := ExpandFileName(ARepoPath);
  
  // Validate repository
  if not DirectoryExists(FRepoPath + '/.git') then
    raise Exception.Create('Not a git repository: ' + FRepoPath);
end;

function TGitToolContext.ExecuteGitCommand(const Args: array of string): string;
var
  Process: TProcess;
  Output: TStringList;
  i: Integer;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'git';
    Process.CurrentDirectory := FRepoPath;
    Process.Options := [poWaitOnExit, poUsePipes];
    
    // Add sanitized arguments
    for i := 0 to High(Args) do
      Process.Parameters.Add(SanitizeGitArgument(Args[i]));
    
    Process.Execute;
    
    if Process.ExitStatus <> 0 then
      raise Exception.CreateFmt('Git command failed with exit code %d', [Process.ExitStatus]);
    
    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
    
  finally
    Output.Free;
    Process.Free;
  end;
end;

function TGitToolContext.SanitizeGitArgument(const Arg: string): string;
begin
  Result := Arg;
  
  // Remove dangerous characters
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);
  Result := StringReplace(Result, '`', '', [rfReplaceAll]);
  Result := StringReplace(Result, '$', '', [rfReplaceAll]);
  
  // Limit length
  if Length(Result) > 255 then
    SetLength(Result, 255);
end;

function TGitToolContext.ValidateGitReference(const Ref: string): Boolean;
begin
  // Basic validation for git references
  Result := (Length(Ref) > 0) and 
            (Length(Ref) <= 255) and
            (not Ref.Contains('..')) and
            (not Ref.StartsWith('-'));
end;

function TGitToolContext.GetGitTags(const Pattern: string; const SortBy: string): string;
var
  Args: array of string;
begin
  SetLength(Args, 3);
  Args[0] := 'tag';
  Args[1] := '-l';
  
  if Pattern <> '' then
    Args[2] := Pattern
  else
    Args[2] := '*';
    
  if SortBy = 'version' then
  begin
    SetLength(Args, Length(Args) + 1);
    Args[High(Args)] := '--sort=-version:refname';
  end
  else if SortBy = 'date' then
  begin
    SetLength(Args, Length(Args) + 1);
    Args[High(Args)] := '--sort=-creatordate';
  end;
  
  Result := ExecuteGitCommand(Args);
end;

function TGitToolContext.GetCommitsBetween(const FromRef, ToRef: string; const Format: string): string;
var
  Args: array of string;
  FormatStr: string;
begin
  if not ValidateGitReference(FromRef) or not ValidateGitReference(ToRef) then
    raise Exception.Create('Invalid git reference');
    
  case Format of
    'oneline': FormatStr := '--oneline';
    'detailed': FormatStr := '--pretty=format:"%H|%an|%ad|%s"';
    else FormatStr := '--pretty=format:"%h %s (%an)"';
  end;
  
  SetLength(Args, 4);
  Args[0] := 'log';
  Args[1] := FormatStr;
  Args[2] := '--no-merges';
  Args[3] := FromRef + '..' + ToRef;
  
  Result := ExecuteGitCommand(Args);
end;

function TGitToolContext.ReadChangelogFile(const FilePath: string): string;
var
  FileStream: TFileStream;
  StringList: TStringList;
  FullPath: string;
begin
  FullPath := FRepoPath + '/' + FilePath;
  
  if not FileExists(FullPath) then
    raise Exception.Create('Changelog file not found: ' + FilePath);
    
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FullPath);
    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

function TGitToolContext.WriteChangelogFile(const Content, FilePath: string): Boolean;
var
  StringList: TStringList;
  FullPath: string;
begin
  Result := False;
  FullPath := FRepoPath + '/' + FilePath;
  
  try
    StringList := TStringList.Create;
    try
      StringList.Text := Content;
      StringList.SaveToFile(FullPath);
      Result := True;
    finally
      StringList.Free;
    end;
  except
    on E: Exception do
      // Log error but don't re-raise
      Result := False;
  end;
end;

function TGitToolContext.GetGitStatus: string;
begin
  Result := ExecuteGitCommand(['status', '--porcelain']);
end;

function TGitToolContext.GetCurrentBranch: string;
begin
  Result := Trim(ExecuteGitCommand(['branch', '--show-current']));
end;

function TGitToolContext.ExecuteTool(const ToolCall: TToolCall): TToolResult;
var
  JsonObj: TJSONObject;
  Pattern, SortBy, FromRef, ToRef, Format, FilePath, Content: string;
begin
  Result.ToolCallId := ToolCall.Id;
  Result.IsError := False;
  
  try
    case ToolCall.FunctionName of
      'get_git_tags':
      begin
        if SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
        try
          Pattern := JsonObj.Get('pattern', '');
          SortBy := JsonObj.Get('sort_by', 'version');
          Result.Content := GetGitTags(Pattern, SortBy);
        finally
          JsonObj.Free;
        end
        else
          Result.Content := GetGitTags();
      end;
      
      'get_commits_between':
      begin
        if not SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
        begin
          Result.Content := 'Invalid JSON arguments';
          Result.IsError := True;
          Exit;
        end;
        
        try
          FromRef := JsonObj.Strings['from_ref'];
          ToRef := JsonObj.Strings['to_ref'];
          Format := JsonObj.Get('format', 'standard');
          Result.Content := GetCommitsBetween(FromRef, ToRef, Format);
        finally
          JsonObj.Free;
        end;
      end;
      
      'read_changelog':
      begin
        if SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
        try
          FilePath := JsonObj.Get('file_path', 'CHANGELOG.md');
          Result.Content := ReadChangelogFile(FilePath);
        finally
          JsonObj.Free;
        end
        else
          Result.Content := ReadChangelogFile();
      end;
      
      'write_changelog':
      begin
        if not SafeParseToolArguments(ToolCall.Arguments, JsonObj) then
        begin
          Result.Content := 'Invalid JSON arguments';
          Result.IsError := True;
          Exit;
        end;
        
        try
          Content := JsonObj.Strings['content'];
          FilePath := JsonObj.Get('file_path', 'CHANGELOG.md');
          
          if WriteChangelogFile(Content, FilePath) then
            Result.Content := 'Successfully wrote ' + FilePath
          else
          begin
            Result.Content := 'Failed to write ' + FilePath;
            Result.IsError := True;
          end;
        finally
          JsonObj.Free;
        end;
      end;
      
      'get_git_status':
      begin
        Result.Content := GetGitStatus;
      end;
      
      'get_current_branch':
      begin
        Result.Content := GetCurrentBranch;
      end;
      
      else
      begin
        Result.Content := 'Unknown tool: ' + ToolCall.FunctionName;
        Result.IsError := True;
      end;
    end;
    
  except
    on E: Exception do
    begin
      Result.Content := 'Tool execution error: ' + E.Message;
      Result.IsError := True;
    end;
  end;
end;

function TGitToolContext.GetAvailableTools: TToolFunctionArray;
begin
  SetLength(Result, 6);
  
  // Tool 1: Get git tags
  Result[0].Name := 'get_git_tags';
  Result[0].Description := 'Get list of git tags with optional filtering and sorting';
  SetLength(Result[0].Parameters, 2);
  
  Result[0].Parameters[0].Name := 'pattern';
  Result[0].Parameters[0].ParamType := tptString;
  Result[0].Parameters[0].Description := 'Optional pattern to filter tags (e.g., "v*")';
  Result[0].Parameters[0].Required := False;
  
  Result[0].Parameters[1].Name := 'sort_by';
  Result[0].Parameters[1].ParamType := tptString;
  Result[0].Parameters[1].Description := 'Sort method: version, date, or name';
  Result[0].Parameters[1].Required := False;
  SetLength(Result[0].Parameters[1].EnumValues, 3);
  Result[0].Parameters[1].EnumValues[0] := 'version';
  Result[0].Parameters[1].EnumValues[1] := 'date';
  Result[0].Parameters[1].EnumValues[2] := 'name';
  
  // Tool 2: Get commits between references
  Result[1].Name := 'get_commits_between';
  Result[1].Description := 'Get commit history between two git references';
  SetLength(Result[1].Parameters, 3);
  
  Result[1].Parameters[0].Name := 'from_ref';
  Result[1].Parameters[0].ParamType := tptString;
  Result[1].Parameters[0].Description := 'Starting reference (exclusive)';
  Result[1].Parameters[0].Required := True;
  
  Result[1].Parameters[1].Name := 'to_ref';
  Result[1].Parameters[1].ParamType := tptString;
  Result[1].Parameters[1].Description := 'Ending reference (inclusive)';
  Result[1].Parameters[1].Required := True;
  
  Result[1].Parameters[2].Name := 'format';
  Result[1].Parameters[2].ParamType := tptString;
  Result[1].Parameters[2].Description := 'Output format';
  Result[1].Parameters[2].Required := False;
  SetLength(Result[1].Parameters[2].EnumValues, 3);
  Result[1].Parameters[2].EnumValues[0] := 'standard';
  Result[1].Parameters[2].EnumValues[1] := 'oneline';
  Result[1].Parameters[2].EnumValues[2] := 'detailed';
  
  // Tool 3: Read changelog
  Result[2].Name := 'read_changelog';
  Result[2].Description := 'Read changelog file content';
  SetLength(Result[2].Parameters, 1);
  
  Result[2].Parameters[0].Name := 'file_path';
  Result[2].Parameters[0].ParamType := tptString;
  Result[2].Parameters[0].Description := 'Path to changelog file';
  Result[2].Parameters[0].Required := False;
  
  // Tool 4: Write changelog
  Result[3].Name := 'write_changelog';
  Result[3].Description := 'Write content to changelog file';
  SetLength(Result[3].Parameters, 2);
  
  Result[3].Parameters[0].Name := 'content';
  Result[3].Parameters[0].ParamType := tptString;
  Result[3].Parameters[0].Description := 'Complete changelog content to write';
  Result[3].Parameters[0].Required := True;
  
  Result[3].Parameters[1].Name := 'file_path';
  Result[3].Parameters[1].ParamType := tptString;
  Result[3].Parameters[1].Description := 'Path to changelog file';
  Result[3].Parameters[1].Required := False;
  
  // Tool 5: Get git status
  Result[4].Name := 'get_git_status';
  Result[4].Description := 'Get current git repository status';
  SetLength(Result[4].Parameters, 0);
  
  // Tool 6: Get current branch
  Result[5].Name := 'get_current_branch';
  Result[5].Description := 'Get name of current git branch';
  SetLength(Result[5].Parameters, 0);
end;

end.
```

## Security Considerations

### Input Sanitization

**Critical**: All user inputs must be sanitized before passing to git commands:

```pascal
function SanitizeGitArgument(const Arg: string): string;
begin
  Result := Arg;
  
  // Remove shell injection characters
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);
  Result := StringReplace(Result, '`', '', [rfReplaceAll]);
  Result := StringReplace(Result, '$', '', [rfReplaceAll]);
  Result := StringReplace(Result, '$(', '', [rfReplaceAll]);
  Result := StringReplace(Result, '${', '', [rfReplaceAll]);
  
  // Limit length to prevent buffer overflow
  if Length(Result) > 255 then
    SetLength(Result, 255);
end;
```

### Reference Validation

```pascal
function ValidateGitReference(const Ref: string): Boolean;
begin
  Result := (Length(Ref) > 0) and 
            (Length(Ref) <= 255) and
            (not Ref.Contains('..')) and        // Prevent path traversal
            (not Ref.StartsWith('-')) and       // Prevent option injection
            (not Ref.Contains(#0)) and          // Prevent null byte injection
            (not Ref.Contains(#10)) and         // Prevent newline injection
            (not Ref.Contains(#13));            // Prevent carriage return injection
end;
```

### File Path Security

```pascal
function ValidateFilePath(const Path: string): Boolean;
var
  NormalizedPath: string;
begin
  NormalizedPath := ExpandFileName(Path);
  
  // Ensure path is within repository
  Result := NormalizedPath.StartsWith(FRepoPath) and
            (not Path.Contains('..')) and
            (not Path.StartsWith('/')) and
            (Length(Path) <= 512);
end;
```

### Process Execution Safety

```pascal
function ExecuteGitCommand(const Args: array of string): string;
var
  Process: TProcess;
  i: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'git';  // Use full path in production
    Process.CurrentDirectory := FRepoPath;
    Process.Options := [poWaitOnExit, poUsePipes, poNoConsole];
    
    // Set timeout to prevent hanging
    Process.StartupOptions := [suoUseShowWindow];
    
    // Validate and add arguments
    for i := 0 to High(Args) do
    begin
      if not ValidateGitArgument(Args[i]) then
        raise Exception.Create('Invalid git argument: ' + Args[i]);
      Process.Parameters.Add(SanitizeGitArgument(Args[i]));
    end;
    
    Process.Execute;
    
    // Check for timeout (implement timeout mechanism)
    if not Process.WaitOnExit(30000) then  // 30 second timeout
    begin
      Process.Terminate(1);
      raise Exception.Create('Git command timed out');
    end;
    
    if Process.ExitStatus <> 0 then
      raise Exception.CreateFmt('Git command failed with exit code %d', [Process.ExitStatus]);
    
    // Read output safely
    Result := ReadProcessOutput(Process);
    
  finally
    Process.Free;
  end;
end;
```

## Error Handling

### Comprehensive Error Management

```pascal
function TGitToolContext.ExecuteTool(const ToolCall: TToolCall): TToolResult;
begin
  Result.ToolCallId := ToolCall.Id;
  Result.IsError := False;
  
  try
    // Validate tool exists
    if not IsValidTool(ToolCall.FunctionName) then
    begin
      Result := CreateToolResult(ToolCall.Id, 
        'Unknown tool: ' + ToolCall.FunctionName, True);
      Exit;
    end;
    
    // Validate repository state
    if not ValidateRepositoryState then
    begin
      Result := CreateToolResult(ToolCall.Id, 
        'Repository is not in a valid state', True);
      Exit;
    end;
    
    // Execute tool with specific error handling
    case ToolCall.FunctionName of
      'get_git_tags': Result := HandleGetGitTags(ToolCall);
      'get_commits_between': Result := HandleGetCommitsBetween(ToolCall);
      // ... other tools
    end;
    
  except
    on E: EGitCommandError do
      Result := CreateToolResult(ToolCall.Id, 
        'Git command failed: ' + E.Message, True);
    on E: EInvalidArgument do
      Result := CreateToolResult(ToolCall.Id, 
        'Invalid argument: ' + E.Message, True);
    on E: EFileNotFound do
      Result := CreateToolResult(ToolCall.Id, 
        'File not found: ' + E.Message, True);
    on E: Exception do
      Result := CreateToolResult(ToolCall.Id, 
        'Unexpected error: ' + E.Message, True);
  end;
end;
```

### Specific Error Types

```pascal
type
  EGitCommandError = class(Exception);
  EInvalidArgument = class(Exception);
  ERepositoryError = class(Exception);
  EFileAccessError = class(Exception);
```

### Repository State Validation

```pascal
function ValidateRepositoryState: Boolean;
begin
  Result := DirectoryExists(FRepoPath + '/.git') and
            FileExists(FRepoPath + '/.git/HEAD') and
            (not IsRepositoryCorrupt);
end;

function IsRepositoryCorrupt: Boolean;
begin
  try
    ExecuteGitCommand(['status', '--porcelain']);
    Result := False;
  except
    Result := True;
  end;
end;
```

## Usage Examples

### Basic Changelog Generation

```pascal
procedure GenerateChangelog;
var
  Provider: TOpenAIProvider;
  ToolContext: TGitToolContext;
  Messages: array of TLLMMessage;
  Tools: TToolFunctionArray;
  Response: TLLMChatCompletionResponse;
begin
  Provider := TOpenAIProvider.Create(GetApiKey);
  ToolContext := TGitToolContext.Create('.');
  try
    // Setup conversation
    SetLength(Messages, 1);
    Messages[0].Role := lmrUser;
    Messages[0].Content := 'Generate a changelog entry for version v1.2.0. ' +
                          'Compare against the previous tag and include all ' +
                          'significant changes with proper formatting.';
    
    Tools := ToolContext.GetAvailableTools;
    
    Response := Provider.ChatCompletionWithTools(
      'gpt-4',
      Messages,
      Tools,
      ToolContext
    );
    
    writeln('Generated changelog:');
    writeln(Response.Choices[0].Message.Content);
    
  finally
    ToolContext.Free;
    Provider.Free;
  end;
end;
```

### Advanced Git Analysis

```pascal
procedure AnalyzeRepository;
var
  UserPrompt: string;
begin
  UserPrompt := 'Analyze this git repository and provide insights:' + sLineBreak +
                '1. How many commits since the last tag?' + sLineBreak +
                '2. What are the main areas of development?' + sLineBreak +
                '3. Are there any patterns in the commit messages?' + sLineBreak +
                '4. Suggest what the next version number should be' + sLineBreak +
                '5. Generate a comprehensive changelog' + sLineBreak + sLineBreak +
                'Use the available git tools to gather all necessary information.';
  
  ExecuteWithTools(UserPrompt);
end;
```

## Testing Strategies

### Unit Testing Structure

```pascal
unit test_git_tools;

interface

uses
  TestFramework, git_tools, models;

type
  TGitToolsTest = class(TTestCase)
  private
    FToolContext: TGitToolContext;
    FTestRepo: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CreateTestRepository;
    procedure CreateTestCommits;
  published
    procedure TestGetGitTags;
    procedure TestGetCommitsBetween;
    procedure TestReadWriteChangelog;
    procedure TestInvalidArguments;
    procedure TestSecurityValidation;
  end;
```

### Integration Testing

```pascal
procedure TestCompleteChangelogGeneration;
var
  Provider: TOpenAIProvider;
  ToolContext: TGitToolContext;
  // ... setup code
begin
  // Test with real OpenAI API
  // Verify complete workflow
  // Check generated changelog format
  // Validate all tools are called correctly
end;
```

### Security Testing

```pascal
procedure TestInputSanitization;
var
  MaliciousInputs: array of string;
  i: Integer;
begin
  MaliciousInputs := [
    '; rm -rf /',
    '& cat /etc/passwd',
    '`id`',
    '$(whoami)',
    '../../../etc/passwd',
    '../../.git/config'
  ];
  
  for i := 0 to High(MaliciousInputs) do
  begin
    // Verify malicious input is properly sanitized
    // Ensure no command injection occurs
    // Check file path validation
  end;
end;
```

## Performance Considerations

### Caching Strategies

```pascal
type
  TGitToolContext = class(TBaseToolContext)
  private
    FTagCache: TStringList;
    FBranchCache: TStringList;
    FCacheTimeout: TDateTime;
    
    function GetCachedTags: string;
    procedure InvalidateCache;
  end;
```

### Async Operations

```pascal
// For large repositories, consider async operations
function GetCommitsBetweenAsync(const FromRef, ToRef: string): TAsyncResult;
```

### Resource Management

```pascal
// Limit concurrent git operations
// Implement operation queuing
// Monitor memory usage for large outputs
```

## Conclusion

This implementation guide provides a comprehensive foundation for git tool calling integration. The modular design allows for incremental implementation, starting with basic tools and expanding to more complex operations.

Key success factors:
- **Security First**: Always sanitize inputs and validate operations
- **Error Resilience**: Comprehensive error handling at every level  
- **Performance**: Cache results and limit resource usage
- **Maintainability**: Clear separation of concerns and good documentation

The tool calling architecture in `models.pas` provides a robust foundation that eliminates string parsing complexity while maintaining security and performance.