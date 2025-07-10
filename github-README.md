# gitpal

> **This is a build mirror. Primary development happens on SourceHut.**

[![Build Status](https://github.com/yourusername/gitpal/workflows/Build%20Release%20Binaries/badge.svg)](https://github.com/yourusername/gitpal/actions)
[![Latest Release](https://img.shields.io/github/v/release/yourusername/gitpal)](https://github.com/yourusername/gitpal/releases)

**Primary Repository**: [https://sr.ht/~matthewp/gitpal/](https://sr.ht/~matthewp/gitpal/)

## Download

Download pre-built binaries from [GitHub Releases](https://github.com/yourusername/gitpal/releases).

### Quick Install

#### Linux/macOS
```bash
# Download latest release (replace with actual URL)
wget https://github.com/yourusername/gitpal/releases/latest/download/gitpal-linux-x86_64

# Make executable and install
chmod +x gitpal-linux-x86_64
sudo mv gitpal-linux-x86_64 /usr/local/bin/gitpal
```

#### macOS (Homebrew)
```bash
brew tap matthewp/gitpal
brew install gitpal
```

#### Windows
1. Download `gitpal-windows-x86_64.exe` from releases
2. Place in a directory in your PATH
3. Rename to `gitpal.exe` if desired

## Development

**This repository is only used for CI/CD and binary releases.**

For patches, bug reports, and discussion, use the SourceHut mailing lists:
- **Development**: [~matthewp/gitpal-devel](https://lists.sr.ht/~matthewp/gitpal-devel)
- **Announcements**: [~matthewp/gitpal-announce](https://lists.sr.ht/~matthewp/gitpal-announce)

## Build from Source

See the [SourceHut repository](https://sr.ht/~matthewp/gitpal/) for build instructions.

## About

gitpal is an AI-powered git assistant CLI tool written in Pascal that analyzes your git changes and generates descriptive commit messages using AI.

**Features:**
- AI-powered commit messages with real-time streaming
- Interactive git undo with AI-assisted operation analysis
- Orphaned commit recovery
- Git operation analysis and explanations
- Multiple LLM support (OpenAI, Claude, Gemini)
- Cross-platform (Linux, macOS, FreeBSD, Windows)

**Primary development**: https://sr.ht/~matthewp/gitpal/