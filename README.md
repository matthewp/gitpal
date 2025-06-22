# gitpal

An AI-powered git assistant CLI tool written in Pascal that analyzes your git changes and generates descriptive commit messages using AI.

## Features

- **AI-powered commit messages**: Analyzes staged changes and generates descriptive commit messages
- **Interactive TUI**: Clean terminal user interface using [bobaui](https://sr.ht/~matthewp/bobaui/)
- **Multiple LLM support**: Connect to various AI providers via [models.pas](https://github.com/matthewjharper/models.pas)
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

1. **Get an API key** from your preferred LLM provider:
   - **Google Gemini**: Set `GEMINI_API_KEY` environment variable
   - **OpenAI**: Set `OPENAI_API_KEY` environment variable
   - **Anthropic Claude**: Set `ANTHROPIC_API_KEY` environment variable

2. **Stage your changes**:
   ```bash
   git add .
   ```

3. **Generate commit message**:
   ```bash
   gitpal commit
   ```

## Usage

```bash
# Generate and apply AI commit message
gitpal commit

# Add custom instructions to the AI prompt
gitpal commit --prompt "Focus on performance improvements"

# Show help
gitpal --help

# Show version
gitpal --version
```

## Development

The project uses:
- **[bobaui](https://sr.ht/~matthewp/bobaui/)** for terminal user interface components
- **[models.pas](https://github.com/matthewjharper/models.pas)** for LLM API integration
- **OpenSSL 1.1** for secure HTTPS connections

### Project Structure

```
src/
└── app.pas              # Main application logic

vendor/
├── bobaui/             # TUI framework
└── models.pas/         # LLM client library
```

## Contributing

This project is hosted on [SourceHut](https://sr.ht/~matthewp/gitpal/). Feel free to submit patches or report issues.
