# gitpal

An AI-powered git assistant CLI tool written in Pascal that analyzes your git changes and generates descriptive commit messages using AI.

**Repository**: [https://sr.ht/~matthewp/gitpal/](https://sr.ht/~matthewp/gitpal/)

## Features

- **AI-powered commit messages**: Analyzes staged changes and generates descriptive commit messages with real-time streaming
- **Interactive git undo**: Safely recover from git mistakes with AI-assisted operation analysis and automated backup creation
- **Interactive setup wizard**: Configure providers, API keys, and models with an intuitive TUI
- **Automated changelog generation**: Update CHANGELOG.md with AI-generated summaries from git history
- **Multiple LLM support**: Choose from OpenAI (GPT-4, GPT-3.5), Anthropic Claude (Sonnet, Haiku), or Google Gemini models
- **OAuth authentication**: Claude Pro/Max users can authenticate without API keys
- **Commit message editing**: Edit generated messages before committing using your preferred editor
- **Interactive TUI**: Clean terminal interface with keyboard shortcuts and vim-style navigation
- **Cross-platform**: Supports macOS, Linux, Windows, and FreeBSD

## Installation

### Package Managers

#### Homebrew (macOS/Linux)

```bash
brew tap matthewp/gitpal https://git.sr.ht/~matthewp/homebrew-gitpal
brew install gitpal
```

## Building from Source

### Prerequisites

- **Free Pascal Compiler (FPC)** 3.2.2 or later
- **OpenSSL 1.1.x** (required for HTTPS API calls)
- **CMake** 3.15 or later

#### macOS with Homebrew

```bash
brew install fpc cmake openssl@1.1
```

#### Linux (Ubuntu/Debian)

```bash
sudo apt-get install fpc cmake libssl-dev
```

### Building

```bash
# Configure the build
cmake -S . -B build

# Build the project
cmake --build build

# Run the binary
./bin/gitpal --help
```

### Build Configuration

The project automatically detects and configures:

- **OpenSSL 1.1**: Required for HTTPS API calls to LLM providers
- **Vendor dependencies**: Includes bobaui (TUI) and models.pas (LLM client)

### Installing from Source

After building, you can install gitpal system-wide:

```bash
# Install to /usr/local/bin (default)
sudo cmake --install build

# Or install to a custom prefix
cmake --install build --prefix /usr/local

# Uninstall
sudo cmake --build build --target uninstall
```

## Setup

gitpal features an interactive setup wizard that runs automatically on first use. You can also run it anytime with `gitpal setup`.

### Quick Start

1. **Run the setup wizard**:
   ```bash
   gitpal setup
   ```

2. **Choose your AI provider**:
   - **OpenAI** - GPT-4 and GPT-3.5 models
   - **Claude** - Anthropic's Claude 3.5 Sonnet and Haiku
   - **Gemini** - Google's Gemini models

3. **Authenticate**:
   - **API Key**: Enter your API key or use environment variables
     - OpenAI: `OPENAI_API_KEY`
     - Claude: `ANTHROPIC_API_KEY`
     - Gemini: `GEMINI_API_KEY`
   - **OAuth** (Claude only): Sign in with your Claude Pro/Max account

4. **Select your model**: Choose from available models for your provider

### Using gitpal

Once configured, using gitpal is simple:

```bash
# Stage your changes
git add .

# Generate commit message
gitpal commit
```

## Usage

### Initial Setup

```bash
# Run interactive setup wizard (automatically runs on first use)
gitpal setup

# During setup you can:
# - Choose your AI provider (OpenAI, Claude, Gemini)
# - Enter API keys or use OAuth (Claude only)
# - Select your preferred model
```

### Generating Commit Messages

```bash
# Generate commit message for staged changes
gitpal commit

# Automatically stage all changes and generate commit message
gitpal commit --stage

# Add custom instructions to guide the AI
gitpal commit --prompt "Focus on the API refactoring"

# Use a different provider for this commit
gitpal commit --provider claude
```

### Updating Changelog

```bash
# Update CHANGELOG.md with all changes since last tag
gitpal changelog

# Generate changelog for a specific version range
gitpal changelog --from v1.0.0 --to v2.0.0

# Use a specific provider
gitpal changelog --provider openai
```

### Recovering from Git Mistakes

```bash
# Interactive recovery from recent git operations
gitpal undo

# Provide context to help AI analyze your specific situation
gitpal undo --prompt "I messed up the rebase"

# Use a specific provider for operation analysis
gitpal undo --provider claude
```

**What gitpal undo can help with:**
- Accidentally ran `git reset --hard` and lost commits
- Botched a rebase and want to go back to the previous state
- Deleted a branch by mistake
- Merged the wrong branch and need to undo it
- Lost commits during complex git operations

**Safety features:**
- Creates automatic backup branches before any recovery
- Shows detailed impact analysis before making changes
- Risk assessment with visual indicators (🟢 LOW, 🟡 MEDIUM, 🔴 HIGH)
- AI-powered explanations of what each operation will do
- Requires confirmation for all recovery operations

### Global Options

```bash
# Show help for any command
gitpal --help
gitpal commit --help
gitpal undo --help

# Show version
gitpal --version
```

## Configuration

gitpal stores its configuration in `~/.config/gitpal/config.json`. You can modify settings by running `gitpal setup` or editing the file directly.

### Available Models

#### OpenAI
- `gpt-4o` - Most capable model with vision and function calling
- `gpt-4o-mini` - Faster, more affordable GPT-4 model
- `gpt-4-turbo` - Previous generation GPT-4
- `gpt-3.5-turbo` - Fast and cost-effective

#### Claude (Anthropic)
- `claude-3-5-sonnet-latest` - Most intelligent Claude model
- `claude-3-5-haiku-latest` - Fast and efficient
- `claude-3-5-sonnet-20241022` - Specific version of Sonnet
- `claude-3-5-haiku-20241022` - Specific version of Haiku

#### Gemini (Google)
- `gemini-2.0-flash-exp` - Latest experimental Flash model
- `gemini-1.5-flash` - Fast multimodal model
- `gemini-1.5-pro` - Most capable Gemini model
- `gemini-1.5-flash-8b` - Efficient 8B parameter model

### Switching Providers

You can override your default provider for any command:

```bash
gitpal commit --provider openai
gitpal changelog --provider claude
```

## Interactive Features

gitpal provides a rich terminal user interface with intuitive keyboard controls.

### Setup Wizard Navigation
- **Arrow keys** or **j/k** - Navigate between options
- **Enter** - Select option
- **q** - Quit setup

### Commit Interface
When generating a commit message:
- **Arrow keys** or **j/k** - Navigate between Accept/Decline
- **Enter** - Confirm selection
- **e** - Edit commit message (opens in your `$EDITOR`)
- **q** - Cancel without committing

### Undo Interface
When recovering from git mistakes:
- **Arrow keys** or **j/k** - Navigate between undoable operations
- **Enter** - Select operation to undo
- **q** - Cancel and exit
- **Real-time progress** - Shows reflog analysis and AI processing
- **Risk indicators** - Color-coded safety levels for each operation
- **Detailed previews** - Shows exactly what each undo will do

### Editor Integration
gitpal respects your environment's editor settings:
- Uses `$EDITOR` or `$VISUAL` environment variables
- Falls back to `nano`, `vim`, or `vi` if available
- Opens with `.gitcommit` extension for syntax highlighting

### Tips
- The AI generates commit messages in real-time with streaming
- Progress spinners show when operations are running
- Use `Ctrl+C` to cancel long-running operations

## Development

The project uses:
- **[bobaui](https://sr.ht/~matthewp/bobaui/)** for terminal user interface components
- **[models.pas](https://github.com/matthewjharper/models.pas)** for LLM API integration
- **OpenSSL 1.1** for secure HTTPS connections

### Project Structure

```
src/
├── gitpal.pas           # Main CLI entry point
├── command_commit.pas   # Commit message generation
├── command_changelog.pas # Changelog generation
└── command_undo.pas     # Interactive git mistake recovery

vendor/
├── bobaui/             # TUI framework
└── models.pas/         # LLM client library
```

## Troubleshooting

### Common Issues

#### OpenSSL Errors
If you encounter SSL/TLS errors when connecting to AI providers:
- **macOS**: Install OpenSSL 1.1 with `brew install openssl@1.1`
- **Linux**: Ensure `libssl-dev` is installed
- FreePascal 3.2.2 requires OpenSSL 1.1.x (not 3.0+)

#### No Editor Opens
If the edit feature doesn't work:
- Set your preferred editor: `export EDITOR=vim`
- gitpal checks for `$EDITOR`, `$VISUAL`, then common editors

#### API Key Not Found
- Run `gitpal setup` to reconfigure
- Check environment variables are exported (not just set)
- Ensure no trailing spaces in API keys

#### Commit Message Not Applied
- Ensure you have staged changes before running `gitpal commit`
- Use `gitpal commit --stage` to automatically stage all changes

#### OAuth Login Issues (Claude)
- Requires active Claude Pro or Max subscription
- Clear cookies/cache if login fails
- Fallback to API key method if needed

#### Undo Command Issues
- **"Repository not clean"**: Commit or stash changes before running `gitpal undo`
- **"No reflog entries found"**: Repository might be new or reflog might be disabled
- **"Operation not found"**: The operation you're looking for might be too old (only shows recent 50 entries)
- **Authentication errors**: Ensure your AI provider is configured with `gitpal setup`

## Contributing

gitpal is developed on [sourcehut](https://sr.ht/~matthewp/gitpal/) using mailing lists for collaboration.

### Mailing Lists

- **Development**: [~matthewp/gitpal-devel](https://lists.sr.ht/~matthewp/gitpal-devel) - Patches, bug reports, and development discussion
- **Announcements**: [~matthewp/gitpal-announce](https://lists.sr.ht/~matthewp/gitpal-announce) - Release announcements and important updates

### Submitting Patches

Send patches to the development mailing list using `git send-email`:

```bash
git send-email --to="~matthewp/gitpal-devel@lists.sr.ht" HEAD^
```

### Debugging

Enable debug logging to help troubleshoot issues:

```bash
GITPAL_DEBUG=1 gitpal commit
```

This creates a `gitpal-debug.log` file in the current directory with detailed execution information.

### Development Tips

- Follow the coding conventions in `CLAUDE.md`
- Test with multiple AI providers before submitting
- Include clear commit messages following conventional commit format
