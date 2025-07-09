program gitpal;

{$mode objfpc}
{$codepage UTF8}
{$H+}

uses
  bobaui,
  SysUtils,
  command_commit,
  command_changelog,
  command_setup,
  command_undo,
  command_recover,
  command_explain,
  config_manager;

const
  AppVersion = '0.6.0';  // Update this when tagging new releases

function EnsureConfigurationExists: Boolean;
var
  Config: TGitPalConfig;
begin
  Result := True;
  Config := TGitPalConfig.Create;
  try
    if not Config.ConfigExists then
    begin
      writeln('gitpal is not configured. Running setup wizard...');
      writeln('');
      RunSetupWizard;
      Result := False; // Don't continue with original command
    end;
  finally
    Config.Free;
  end;
end;

procedure ShowHelp;
begin
  writeln('gitpal - AI-powered git assistant');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal [command]');
  writeln('');
  writeln('Available Commands:');
  writeln('  setup        Configure gitpal with your AI provider');
  writeln('  commit       Generate and apply AI-powered commit messages');
  writeln('  changelog    Update CHANGELOG.md with recent changes');
  writeln('  undo         Interactively undo recent git operations');
  writeln('  recover      Find and restore lost commits and work');
  writeln('  explain      Understand what happened during git operations');
  writeln('');
  writeln('Options:');
  writeln('  --help, -h     Show this help message');
  writeln('  --version, -v  Show version information');
  writeln('');
  writeln('Use "gitpal [command] --help" for more information about a command.');
end;

procedure ShowCommitHelp;
begin
  writeln('gitpal commit - Generate AI-powered commit messages');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal commit [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes your git changes and generates a descriptive commit message');
  writeln('  using AI. You can review and accept or decline the suggestion.');
  writeln('');
  writeln('Options:');
  writeln('  --stage              Stage all changes before generating commit message');
  writeln('  --prompt <text>      Add custom instructions to the AI prompt');
  writeln('  --provider <name>    Override default provider (openai, claude, gemini)');
  writeln('  --help, -h           Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal commit');
  writeln('  gitpal commit --stage');
  writeln('  gitpal commit --provider claude');
  writeln('  gitpal commit --stage --prompt "Focus on performance improvements"');
  writeln('  gitpal commit --prompt "Don''t mention specific technology names"');
end;

procedure ShowSetupHelp;
begin
  writeln('gitpal setup - Configure gitpal with your AI provider');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal setup [options]');
  writeln('');
  writeln('Description:');
  writeln('  Interactive wizard to configure gitpal with OpenAI, Claude, or Gemini.');
  writeln('  Guides you through provider selection, API key setup, and preferences.');
  writeln('');
  writeln('Options:');
  writeln('  --help, -h   Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal setup         # Start interactive setup wizard');
end;

procedure ShowChangelogHelp;
begin
  writeln('gitpal changelog - Update CHANGELOG.md file');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal changelog [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes recent commits and updates your CHANGELOG.md file with');
  writeln('  a summary of changes organized by type (features, fixes, etc.).');
  writeln('');
  writeln('Options:');
  writeln('  --from <tag>         Start from this tag/version (e.g., v1.0.0)');
  writeln('  --to <tag>           End at this tag/version (e.g., v2.0.0)');
  writeln('  --provider <name>    Override default provider (openai, claude, gemini)');
  writeln('  --help, -h           Show this help message');
  writeln('');
  writeln('Note: By default, processes up to 5 most recent missing versions.');
end;

procedure ShowUndoHelp;
begin
  writeln('gitpal undo - Interactively undo recent git operations');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal undo [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes recent git operations and provides an interactive interface');
  writeln('  to safely undo destructive operations like resets, rebases, and merges.');
  writeln('  Creates backup branches before making changes for safety.');
  writeln('');
  writeln('Options:');
  writeln('  --prompt <text>      Filter operations by description (e.g., "rebase")');
  writeln('  --help, -h           Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal undo');
  writeln('  gitpal undo --prompt "I messed up the rebase"');
  writeln('  gitpal undo --prompt "lost my work on login feature"');
end;

procedure ShowRecoverHelp;
begin
  writeln('gitpal recover - Find and restore lost commits and work');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal recover [options]');
  writeln('');
  writeln('Description:');
  writeln('  Searches for orphaned commits that are no longer reachable from any');
  writeln('  branch and provides an interactive interface to recover lost work.');
  writeln('  Uses AI to group related commits and suggest recovery strategies.');
  writeln('');
  writeln('Options:');
  writeln('  --prompt <text>      Describe what you''re looking for (e.g., "user auth")');
  writeln('  --provider <name>    Override default provider (openai, claude, gemini)');
  writeln('  --help, -h           Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal recover');
  writeln('  gitpal recover --prompt "lost my work on user authentication"');
  writeln('  gitpal recover --prompt "OAuth integration feature"');
  writeln('');
  writeln('Recovery Options:');
  writeln(AnsiString('  • Create new branch with recovered commits'));
  writeln(AnsiString('  • Cherry-pick specific commits to current branch'));
  writeln(AnsiString('  • Show diffs to examine lost work before recovery'));
end;

procedure ShowExplainHelp;
begin
  writeln('gitpal explain - Understand what happened during git operations');
  writeln('');
  writeln('Usage:');
  writeln('  gitpal explain [options]');
  writeln('');
  writeln('Description:');
  writeln('  Analyzes recent git operations and provides AI-powered explanations');
  writeln('  of what changed and why. Helps understand the impact of complex');
  writeln('  operations like rebases, merges, and resets.');
  writeln('');
  writeln('Options:');
  writeln('  --provider <name>    Override default provider (openai, claude, gemini)');
  writeln('  --help, -h           Show this help message');
  writeln('');
  writeln('Examples:');
  writeln('  gitpal explain');
  writeln('  gitpal explain --provider claude');
  writeln('');
  writeln('Output includes:');
  writeln(AnsiString('  • Before/after analysis of recent operations'));
  writeln(AnsiString('  • Explanation of what changed and why'));
  writeln(AnsiString('  • Context for complex multi-step operations'));
end;

var
  Command: string;
  CustomPrompt: string;
  StageChanges: Boolean;
  ProviderOverride: string;
  FromTag: string;
  ToTag: string;
  i: integer;
  ShowMainHelp: boolean;
begin
  Command := '';
  CustomPrompt := '';
  StageChanges := False;
  ProviderOverride := '';
  FromTag := '';
  ToTag := '';
  ShowMainHelp := False;
  
  // Parse command line arguments
  if ParamCount = 0 then
  begin
    ShowMainHelp := True;
  end
  else
  begin
    i := 1;
    while i <= ParamCount do
    begin
      if (ParamStr(i) = '--help') or (ParamStr(i) = '-h') then
      begin
        if Command = AnsiString('') then
          ShowMainHelp := True
        else if Command = AnsiString('commit') then
        begin
          ShowCommitHelp;
          Exit;
        end
        else if Command = AnsiString('setup') then
        begin
          ShowSetupHelp;
          Exit;
        end
        else if Command = AnsiString('changelog') then
        begin
          ShowChangelogHelp;
          Exit;
        end
        else if Command = AnsiString('undo') then
        begin
          ShowUndoHelp;
          Exit;
        end
        else if Command = AnsiString('recover') then
        begin
          ShowRecoverHelp;
          Exit;
        end
        else if Command = AnsiString('explain') then
        begin
          ShowExplainHelp;
          Exit;
        end;
        Break;
      end
      else if (ParamStr(i) = '--version') or (ParamStr(i) = '-v') then
      begin
        writeln(AppVersion);
        Exit;
      end
      else if ParamStr(i) = '--prompt' then
      begin
        // Get the next parameter as the prompt value
        if i < ParamCount then
        begin
          Inc(i);
          CustomPrompt := AnsiString(ParamStr(i));
        end
        else
        begin
          writeln('Error: --prompt flag requires a value');
          Halt(1);
        end;
      end
      else if ParamStr(i) = '--stage' then
      begin
        StageChanges := True;
      end
      else if ParamStr(i) = '--provider' then
      begin
        // Get the next parameter as the provider value
        if i < ParamCount then
        begin
          Inc(i);
          ProviderOverride := AnsiString(ParamStr(i));
        end
        else
        begin
          writeln('Error: --provider flag requires a value');
          Halt(1);
        end;
      end
      else if ParamStr(i) = '--from' then
      begin
        // Get the next parameter as the from tag value
        if i < ParamCount then
        begin
          Inc(i);
          FromTag := AnsiString(ParamStr(i));
        end
        else
        begin
          writeln('Error: --from flag requires a value');
          Halt(1);
        end;
      end
      else if ParamStr(i) = '--to' then
      begin
        // Get the next parameter as the to tag value
        if i < ParamCount then
        begin
          Inc(i);
          ToTag := AnsiString(ParamStr(i));
        end
        else
        begin
          writeln('Error: --to flag requires a value');
          Halt(1);
        end;
      end
      else if (Command = AnsiString('')) and (ParamStr(i)[1] <> '-') then
        Command := AnsiString(ParamStr(i))
      else if (Command = AnsiString('commit')) and (ParamStr(i)[1] <> '-') then
      begin
        // This is a custom prompt for commit command (old style)
        CustomPrompt := AnsiString(ParamStr(i));
      end;
      
      Inc(i);
    end;
  end;
  
  // Show help or execute command
  if ShowMainHelp then
  begin
    ShowHelp;
    Exit;
  end;
  
  if Command = AnsiString('setup') then
  begin
    RunSetupWizard;
  end
  else if Command = AnsiString('commit') then
  begin
    if EnsureConfigurationExists then
      RunCommitCommand(CustomPrompt, StageChanges, ProviderOverride);
  end
  else if Command = AnsiString('changelog') then
  begin
    if EnsureConfigurationExists then
      RunChangelogCommand(ProviderOverride, FromTag, ToTag);
  end
  else if Command = AnsiString('undo') then
  begin
    RunUndoCommand(CustomPrompt);
  end
  else if Command = AnsiString('recover') then
  begin
    if EnsureConfigurationExists then
      RunRecoverCommand(CustomPrompt, ProviderOverride);
  end
  else if Command = AnsiString('explain') then
  begin
    if EnsureConfigurationExists then
      RunExplainCommand(ProviderOverride);
  end
  else
  begin
    writeln('Error: Unknown command "', Command, '"');
    writeln('');
    ShowHelp;
    Halt(1);
  end;
end.
