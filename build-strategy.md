# Cross-Platform Build Strategy for gitpal

## Overall Strategy: SourceHut → GitHub Release Pipeline

### Philosophy
- **SourceHut remains primary**: All development, patches, discussions happen on SourceHut
- **GitHub for CI/CD only**: Used purely for cross-platform builds and binary distribution
- **Minimal GitHub footprint**: Only triggered on releases, no ongoing mirroring
- **Clear separation**: SourceHut for development, GitHub for distribution

### Release Flow
```
SourceHut (Primary Development) 
    ↓ (git tag v0.6.0 && git push origin v0.6.0)
SourceHut Build (.builds/release.yml)
    ↓ (pushes tag to GitHub)
GitHub Actions (triggered by tag)
    ↓ (cross-platform matrix builds)
GitHub Releases (binaries + Homebrew bottles)
```

### Implementation Phases
1. **Phase 1**: Set up GitHub Actions workflow (manual testing)
2. **Phase 2**: Test cross-platform builds and binary packaging
3. **Phase 3**: Add SourceHut build automation
4. **Phase 4**: Integrate with Homebrew bottles

## Phase 1: GitHub Actions Setup (Manual Testing)

### GitHub Repository Setup
Create a minimal GitHub repository at `github.com/yourusername/gitpal` with:

#### Required Files
- **README.md** - Points back to SourceHut as primary
- **.github/workflows/build.yml** - CI workflow
- **LICENSE** - Copy from SourceHut

#### README.md for GitHub
```markdown
# gitpal

> **This is a build mirror. Primary development happens on SourceHut.**

[![Build Status](https://github.com/yourusername/gitpal/workflows/Build/badge.svg)](https://github.com/yourusername/gitpal/actions)

**Primary Repository**: [https://sr.ht/~matthewp/gitpal/](https://sr.ht/~matthewp/gitpal/)

## Download

Download pre-built binaries from [GitHub Releases](https://github.com/yourusername/gitpal/releases).

## Development

For patches, bug reports, and discussion, use the SourceHut mailing lists:
- **Development**: [~matthewp/gitpal-devel](https://lists.sr.ht/~matthewp/gitpal-devel)
- **Announcements**: [~matthewp/gitpal-announce](https://lists.sr.ht/~matthewp/gitpal-announce)

## Build from Source

See the [SourceHut repository](https://sr.ht/~matthewp/gitpal/) for build instructions.
```

### Manual Testing Workflow
Before automating with SourceHut, test the GitHub Actions by:

1. **Initial push to GitHub**:
   ```bash
   # Add GitHub remote to your SourceHut repository
   git remote add github https://github.com/yourusername/gitpal.git
   
   # Push current main branch
   git push github main
   ```

2. **Test with a development tag**:
   ```bash
   # Create a test tag
   git tag v0.6.0-test
   git push github v0.6.0-test
   
   # This should trigger GitHub Actions
   ```

3. **Verify the build**:
   - Check GitHub Actions logs
   - Verify all platforms build successfully
   - Check that release is created with binaries
   - Test downloading and running binaries

4. **Iterate on the workflow**:
   - Fix any platform-specific build issues
   - Adjust binary packaging and naming
   - Test OpenSSL bundling on each platform

## OpenSSL Distribution Strategy

### Linux
```bash
# Bundle OpenSSL 1.1 libraries with the binary
ldd bin/gitpal  # Check dependencies
cp /usr/lib/x86_64-linux-gnu/libssl.so.1.1 ./dist/
cp /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1 ./dist/
```

### macOS  
```bash
# Use install_name_tool to set rpath for bundled libraries
install_name_tool -add_rpath @executable_path/../lib bin/gitpal
# Or rely on Homebrew's openssl@1.1 for Homebrew distribution
```

### Windows
```bash
# Bundle OpenSSL DLLs (download from https://slproweb.com/products/Win32OpenSSL.html)
# Place in same directory as gitpal.exe:
# - libssl-1_1-x64.dll
# - libcrypto-1_1-x64.dll
```

## CI/CD Implementation

### Two-Stage Pipeline

#### Stage 1: SourceHut Build (Trigger)
```yaml
# .builds/release.yml
image: archlinux
packages:
  - git
  - github-cli
secrets:
  - github-token  # GitHub Personal Access Token
environment:
  GITHUB_REPO: yourusername/gitpal
tasks:
  - trigger-github-build: |
      cd gitpal
      
      # Only run on tags
      if ! git describe --exact-match --tags HEAD >/dev/null 2>&1; then
        echo "Not a tag, skipping GitHub trigger"
        exit 0
      fi
      
      TAG=$(git describe --exact-match --tags HEAD)
      echo "Triggering GitHub build for tag: $TAG"
      
      # Push tag to GitHub
      git remote add github https://github.com/$GITHUB_REPO.git
      echo "$GITHUB_TOKEN" | gh auth login --with-token
      git push github HEAD:main
      git push github $TAG
```

#### Stage 2: GitHub Actions (Cross-Platform Builds)
```yaml
# .github/workflows/build.yml
name: Build Release Binaries
on:
  push:
    tags: ['v*']
  workflow_dispatch:  # Allow manual trigger for testing

jobs:
  build:
    strategy:
      fail-fast: false
      matrix:
        include:
          - os: ubuntu-20.04
            target: linux-x86_64
            
          - os: macos-12
            target: darwin-x86_64
            
          - os: macos-14
            target: darwin-arm64
            
          - os: windows-2022
            target: windows-x86_64
            
          - os: freebsd-13
            target: freebsd-x86_64
            # Note: FreeBSD requires third-party action or VM
    
    runs-on: ${{ matrix.os }}
    
    steps:
      - name: Checkout
        uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Full history for git describe
        
      - name: Install Dependencies (Linux)
        if: runner.os == 'Linux'
        run: |
          sudo apt-get update
          sudo apt-get install -y cmake libssl-dev
          
          # Install FreePascal from official package
          wget -O - https://www.freepascal.org/keys/key.asc | gpg --dearmor | sudo tee /usr/share/keyrings/freepascal-keyring.gpg > /dev/null
          echo "deb [signed-by=/usr/share/keyrings/freepascal-keyring.gpg] https://www.freepascal.org/packages/ubuntu jammy main" | sudo tee /etc/apt/sources.list.d/freepascal.list
          sudo apt-get update
          sudo apt-get install -y fpc
          
      - name: Install Dependencies (macOS)
        if: runner.os == 'macOS'
        run: |
          brew install fpc cmake openssl@1.1
          
      - name: Install Dependencies (Windows)
        if: runner.os == 'Windows'
        run: |
          # Download and install FreePascal
          Invoke-WebRequest -Uri "https://downloads.freepascal.org/fpc/dist/3.2.2/i386-win32/fpc-3.2.2.i386-win32.exe" -OutFile "fpc-installer.exe"
          Start-Process -FilePath "fpc-installer.exe" -ArgumentList "/SILENT" -Wait
          
          # Download OpenSSL 1.1 binaries
          Invoke-WebRequest -Uri "https://slproweb.com/download/Win64OpenSSL_Light-1_1_1w.exe" -OutFile "openssl-installer.exe"
          Start-Process -FilePath "openssl-installer.exe" -ArgumentList "/SILENT" -Wait
          
      - name: Install Dependencies (FreeBSD)
        if: runner.os == 'FreeBSD'
        run: |
          # Install FreePascal and dependencies using pkg
          sudo pkg update
          sudo pkg install -y fpc cmake openssl111
          
          # Set up environment for OpenSSL 1.1
          echo "OPENSSL_ROOT_DIR=/usr/local" >> $GITHUB_ENV
          
      - name: Build
        run: |
          cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
          cmake --build build
          
      - name: Package with Dependencies
        shell: bash
        run: |
          mkdir -p dist
          
          if [ "$RUNNER_OS" == "Linux" ]; then
            cp bin/gitpal dist/gitpal-linux-x86_64
            # Bundle OpenSSL libraries
            ldd bin/gitpal | grep -E "libssl|libcrypto" | awk '{print $3}' | xargs -I {} cp {} dist/
          elif [ "$RUNNER_OS" == "macOS" ]; then
            cp bin/gitpal dist/gitpal-${{ matrix.target }}
            # Set rpath for Homebrew OpenSSL
            install_name_tool -add_rpath /opt/homebrew/opt/openssl@1.1/lib dist/gitpal-${{ matrix.target }} 2>/dev/null || true
            install_name_tool -add_rpath /usr/local/opt/openssl@1.1/lib dist/gitpal-${{ matrix.target }} 2>/dev/null || true
          elif [ "$RUNNER_OS" == "Windows" ]; then
            cp bin/gitpal.exe dist/gitpal-windows-x86_64.exe
            # Bundle OpenSSL DLLs
            cp "C:/Program Files/OpenSSL-Win64/bin/libssl-1_1-x64.dll" dist/
            cp "C:/Program Files/OpenSSL-Win64/bin/libcrypto-1_1-x64.dll" dist/
          elif [ "$RUNNER_OS" == "FreeBSD" ]; then
            cp bin/gitpal dist/gitpal-freebsd-x86_64
            # Bundle OpenSSL libraries
            ldd bin/gitpal | grep -E "libssl|libcrypto" | awk '{print $3}' | xargs -I {} cp {} dist/ || true
          fi
          
      - name: Upload Artifacts
        uses: actions/upload-artifact@v4
        with:
          name: gitpal-${{ matrix.target }}
          path: dist/
          
  release:
    needs: build
    runs-on: ubuntu-latest
    if: startsWith(github.ref, 'refs/tags/v')
    
    steps:
      - name: Download All Artifacts
        uses: actions/download-artifact@v4
        
      - name: Create Release
        uses: softprops/action-gh-release@v1
        with:
          files: "*/gitpal*"
          name: "gitpal ${{ github.ref_name }}"
          body: |
            Binary release for gitpal ${{ github.ref_name }}
            
            **Primary development**: https://sr.ht/~matthewp/gitpal/
            
            ## Downloads
            - **Linux**: `gitpal-linux-x86_64`
            - **macOS (Intel)**: `gitpal-darwin-x86_64`
            - **macOS (Apple Silicon)**: `gitpal-darwin-arm64`
            - **FreeBSD**: `gitpal-freebsd-x86_64`
            - **Windows**: `gitpal-windows-x86_64.exe`
```

## Homebrew Integration Strategy

### Current Issue
Homebrew currently builds from source on user machines, which is slow and requires all build dependencies.

### Solution: Use GitHub Release Binaries
With the SourceHut → GitHub pipeline, we can simplify Homebrew integration:

```ruby
class Gitpal < Formula
  desc "AI-powered git assistant CLI tool"
  homepage "https://sr.ht/~matthewp/gitpal/"
  
  # Use GitHub release binaries directly
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/yourusername/gitpal/releases/download/v0.6.0/gitpal-darwin-arm64"
      sha256 "sha256hash"
    else
      url "https://github.com/yourusername/gitpal/releases/download/v0.6.0/gitpal-darwin-x86_64"
      sha256 "sha256hash"
    end
  end
  
  on_linux do
    url "https://github.com/yourusername/gitpal/releases/download/v0.6.0/gitpal-linux-x86_64"
    sha256 "sha256hash"
  end
  
  on_freebsd do
    url "https://github.com/yourusername/gitpal/releases/download/v0.6.0/gitpal-freebsd-x86_64"
    sha256 "sha256hash"
  end
  
  license "MIT"
  
  # Runtime dependencies only (no build dependencies needed)
  depends_on "openssl@1.1"
  
  def install
    bin.install "gitpal-#{OS.kernel_name.downcase}-#{Hardware::CPU.arch}" => "gitpal"
  end
  
  def caveats
    <<~EOS
      Primary development: https://sr.ht/~matthewp/gitpal/
      For patches and discussion: https://lists.sr.ht/~matthewp/gitpal-devel
    EOS
  end
  
  test do
    system "#{bin}/gitpal", "--version"
  end
end
```

### Benefits of This Approach
- ✅ **No build dependencies** (fpc, cmake) required on user machines
- ✅ **Faster installation** - just downloads pre-built binary
- ✅ **Consistent builds** - same binary that was tested in CI
- ✅ **Smaller formula** - no complex build logic needed
- ✅ **Clear attribution** - points back to SourceHut for development

## Windows-Specific Considerations

### FreePascal on Windows
- Download from: https://www.freepascal.org/download.html
- Use 64-bit version for consistency
- May need Visual Studio Build Tools for some dependencies

### OpenSSL on Windows  
- Download from: https://slproweb.com/products/Win32OpenSSL.html
- Use "Win64 OpenSSL v1.1.1w" (latest 1.1.x)
- Extract DLLs: `libssl-1_1-x64.dll`, `libcrypto-1_1-x64.dll`

### CMake Configuration for Windows
```cmake
# Windows-specific OpenSSL discovery
if(WIN32)
    # Look for OpenSSL in common Windows locations
    set(OPENSSL_ROOT_DIR "C:/Program Files/OpenSSL-Win64")
    set(OPENSSL_USE_STATIC_LIBS FALSE)
endif()
```

## Distribution Channels

### GitHub Releases (Primary)
- Automated releases from CI
- Pre-built binaries for all platforms
- Clear naming: `gitpal-v0.6.0-linux-x86_64.tar.gz`

### Package Managers
- **macOS**: Homebrew (with bottles)
- **Linux**: Create .deb/.rpm packages, consider Snap/Flatpak
- **Windows**: Consider Chocolatey, winget

### Direct Downloads
- Simple download page with platform detection
- Installation scripts for each platform