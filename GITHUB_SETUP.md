# GitHub Release Setup Guide

This guide walks you through setting up the GitHub Actions workflow for cross-platform binary releases.

## Step 1: Create GitHub Repository

1. Go to GitHub and create a new repository: `github.com/yourusername/gitpal`
2. **Important**: Replace `yourusername` with your actual GitHub username in all files
3. Make it public (required for free GitHub Actions)
4. Don't initialize with README (we'll push from SourceHut)

## Step 2: Set Up Local Repository

```bash
# Add GitHub as a remote to your SourceHut repository
git remote add github https://github.com/yourusername/gitpal.git

# Push current code to GitHub
git push github main
```

## Step 3: Add GitHub Files

```bash
# Copy the GitHub workflow file
cp .github/workflows/build.yml .github/workflows/build.yml

# Copy the GitHub README
cp github-README.md README.md

# Update the README with your actual GitHub username
sed -i 's/yourusername/YOUR_ACTUAL_USERNAME/g' README.md
sed -i 's/yourusername/YOUR_ACTUAL_USERNAME/g' .github/workflows/build.yml

# Commit and push
git add .github/ README.md
git commit -m "Add GitHub Actions workflow and README"
git push github main
```

## Step 4: Test the Workflow

### Manual Trigger Test
1. Go to your GitHub repository
2. Click on "Actions" tab
3. Select "Build Release Binaries"
4. Click "Run workflow" button
5. Choose the `main` branch and click "Run workflow"

This will test the build process without creating a release.

### Tag-based Release Test
```bash
# Create a test tag
git tag v0.6.0-test
git push github v0.6.0-test
```

This should:
1. Trigger the GitHub Actions workflow
2. Build binaries for all platforms
3. Create a GitHub release with the binaries

## Step 5: Verify the Release

1. Go to your GitHub repository
2. Click on "Releases" (or go to `/releases`)
3. You should see a new release `v0.6.0-test`
4. Download each binary and test:
   - Linux: `gitpal-linux-x86_64`
   - macOS Intel: `gitpal-darwin-x86_64`
   - macOS Apple Silicon: `gitpal-darwin-arm64`
   - FreeBSD: `gitpal-freebsd-x86_64`
   - Windows: `gitpal-windows-x86_64.exe`

## Step 6: Test the Binaries

```bash
# Make executable (Linux/macOS)
chmod +x gitpal-linux-x86_64

# Test basic functionality
./gitpal-linux-x86_64 --version
./gitpal-linux-x86_64 --help
```

## Troubleshooting

### Common Issues

#### FreePascal Installation Fails
- **Linux**: The FPC repository might not be available for all Ubuntu versions
- **Windows**: The silent installer might not work correctly
- **macOS**: Homebrew should work reliably

#### OpenSSL Library Issues
- **Linux**: Libraries might not be bundled correctly
- **macOS**: rpath settings might need adjustment
- **Windows**: DLL paths might be incorrect

#### Build Timeout
- Windows builds might be slow due to downloading installers
- Consider caching dependencies in future iterations

### Debug Steps

1. **Check GitHub Actions logs**: Click on the failed workflow run to see detailed logs
2. **Test individual steps**: Use the "Run workflow" button to test without creating releases
3. **Check file permissions**: Ensure binaries are executable after download

## Next Steps

Once the GitHub Actions workflow is working:

1. **Update build-strategy.md** with any needed changes
2. **Set up SourceHut build automation** (Phase 3)
3. **Update Homebrew formula** to use GitHub releases
4. **Add more platforms** if needed (FreeBSD, etc.)

## Important Notes

- **Version string**: Update the version in `src/gitpal.pas` before tagging
- **Security**: GitHub releases are public - don't include sensitive information
- **Naming**: Keep binary names consistent for easy automation
- **Testing**: Always test downloads on actual target platforms

## File Structure

After setup, your GitHub repository should have:
```
├── .github/
│   └── workflows/
│       └── build.yml
├── README.md (GitHub-specific)
├── LICENSE (copied from SourceHut)
└── (source code pushed from SourceHut)
```