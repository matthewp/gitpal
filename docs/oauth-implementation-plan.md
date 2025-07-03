# GitPal OAuth Authentication Implementation Plan

## Overview

This document outlines the implementation plan for adding OAuth 2.0 authentication to GitPal, allowing users to authenticate with Google Gemini without manually setting the `GEMINI_API_KEY` environment variable.

## Background Research

### Research Results: OAuth is Supported!

**✅ CONFIRMED**: Google supports OAuth 2.0 authentication for Gemini API access through their Code Assist service.

Analysis of Google's official `gemini-cli` tool reveals:

1. **OAuth is the primary authentication method** for Gemini Code Assist
2. **Specific OAuth credentials are provided** by Google for public use in installed applications
3. **Complete implementation reference** exists in the official CLI source code
4. **API endpoints and integration patterns** are documented through working code

### Official Google Implementation Analysis

Based on `google-gemini/gemini-cli` repository analysis:

- **OAuth Client**: Uses Google's `google-auth-library` with proven OAuth2Client implementation
- **Authentication Flow**: Web-based OAuth with local HTTP server callback (identical to our planned approach)
- **Token Management**: Automatic token refresh, secure credential caching, server-side validation
- **API Integration**: OAuth tokens authenticate via standard Bearer headers to Google's Code Assist endpoints

### CLI OAuth Best Practices
Based on research and Google's official implementation, the standard OAuth flow for CLI applications involves:

1. **Local Callback Server**: Start a temporary HTTP server on localhost to receive the OAuth callback
2. **Browser-Based Authentication**: Direct users to authenticate in their default browser
3. **State Parameter Security**: Use random state parameter to prevent CSRF attacks (Google doesn't use PKCE)
4. **Secure Token Storage**: Store tokens with appropriate file permissions
5. **Dynamic Port Allocation**: Find available localhost port to avoid conflicts

### Key Security Requirements
- Bind server only to localhost (127.0.0.1)
- Implement state parameter verification for CSRF protection
- Store tokens with 0600 permissions (owner read/write only)
- Validate tokens both locally and server-side

## Implementation Design

### 1. OAuth Flow Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   GitPal    │────▶│   Browser   │────▶│   Google    │
│     CLI     │     │             │     │    OAuth    │
└─────────────┘     └─────────────┘     └─────────────┘
       ▲                    │                    │
       │                    │                    │
       │              Redirect with              │
       │              auth code                  │
       │                    │                    │
       │                    ▼                    │
       │            ┌─────────────┐              │
       └────────────│ Local HTTP  │◀─────────────┘
                    │   Server     │   Token Exchange
                    └─────────────┘

```

### 2. Components to Implement

#### A. Local HTTP Server Module (`src/oauth_server.pas`)
- Start HTTP server on dynamically allocated localhost port
- Handle OAuth callback at `/oauth2callback` (matching Google's pattern)
- Extract authorization code from query parameters
- Verify state parameter against stored value
- Redirect to success/failure URLs
- Shut down after successful callback

#### B. OAuth Client Module (`src/oauth_client.pas`)
- Generate random state parameter for CSRF protection
- Build authorization URL with Google's specific parameters
- Exchange authorization code for access token using client secret
- Handle automatic token refresh logic
- Validate tokens both locally and with Google's token info endpoint

#### C. Token Storage Module (`src/token_storage.pas`)
- Store tokens in `$XDG_CONFIG_HOME/gitpal/oauth_creds.json` (follows XDG standards)
- Fallback to `$HOME/.config/gitpal/oauth_creds.json` if XDG_CONFIG_HOME not set
- Implement secure file permissions (0600)
- Support both cached credentials and `GOOGLE_APPLICATION_CREDENTIALS` env var
- Provide token retrieval, validation, and refresh methods
- Handle token expiration with automatic refresh

#### D. Browser Launch Module (`src/browser_launch.pas`)
- Cross-platform browser opening (Linux, macOS, Windows)
- Fallback to displaying URL for manual copy/paste

### 3. OAuth Configuration

#### Verified Google OAuth 2.0 Parameters
```pascal
const
  // Official Google OAuth credentials for installed applications
  // Source: google-gemini/gemini-cli (public repository)
  OAUTH_CLIENT_ID = '681255809395-oo8ft2oprdrnp9e3aqf6av3hmdib135j.apps.googleusercontent.com';
  OAUTH_CLIENT_SECRET = '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';
  
  // OAuth endpoints
  OAUTH_AUTH_ENDPOINT = 'https://accounts.google.com/o/oauth2/v2/auth';
  OAUTH_TOKEN_ENDPOINT = 'https://oauth2.googleapis.com/token';
  
  // Required scopes for Gemini Code Assist API access
  OAUTH_SCOPES: array[0..2] of string = (
    'https://www.googleapis.com/auth/cloud-platform',
    'https://www.googleapis.com/auth/userinfo.email',
    'https://www.googleapis.com/auth/userinfo.profile'
  );
  
  // Dynamic redirect URI (port determined at runtime)
  OAUTH_REDIRECT_PATH = '/oauth2callback';
  
  // Success/failure redirect URLs
  OAUTH_SUCCESS_URL = 'https://developers.google.com/gemini-code-assist/auth_success_gemini';
  OAUTH_FAILURE_URL = 'https://developers.google.com/gemini-code-assist/auth_failure_gemini';
```

#### Gemini API Configuration
```pascal
const
  // Gemini Code Assist API endpoints
  GEMINI_API_ENDPOINT = 'https://cloudcode-pa.googleapis.com';
  GEMINI_API_VERSION = 'v1internal';
  
  // Token storage (following XDG Base Directory Specification)
  GITPAL_CONFIG_DIR = 'gitpal';
  OAUTH_CREDS_FILE = 'oauth_creds.json';
```

### 4. User Flow

1. User runs `gitpal auth` command
2. GitPal starts local HTTP server
3. GitPal opens browser to Google OAuth consent page
4. User authenticates and approves permissions
5. Google redirects to localhost callback
6. GitPal captures authorization code
7. GitPal exchanges code for access token
8. Token is securely stored for future use

### 5. File Structure

```
src/
├── oauth_server.pas      # Local HTTP server for OAuth callback
├── oauth_client.pas      # OAuth client implementation
├── token_storage.pas     # Secure token storage
├── browser_launch.pas    # Cross-platform browser launching
└── command_auth.pas      # Auth command implementation
```

### 6. Dependencies

#### Pascal Libraries Needed
- `fphttpclient`: For HTTP requests to OAuth endpoints
- `httpdefs`, `fphttpserver`: For local callback server
- `fpjson`: For parsing OAuth responses
- `opensslsockets`: For HTTPS support

### 7. Implementation Steps

1. **Phase 1: Core OAuth Infrastructure**
   - Implement PKCE generation
   - Build OAuth authorization URL
   - Create local HTTP server for callback

2. **Phase 2: Token Management**
   - Implement secure token storage
   - Add token refresh logic
   - Create token validation methods

3. **Phase 3: Integration**
   - Modify `models.pas` to authenticate with Gemini Code Assist API
   - Add `auth` command to CLI
   - Update API endpoint from standard Gemini to Code Assist service
   - Implement Bearer token authentication headers

4. **Phase 4: Polish**
   - Add error handling and recovery
   - Implement timeout for auth flow
   - Add status messages and progress indicators

### 8. Security Considerations

1. **State Parameter Implementation** (Google uses state, not PKCE)
   ```pascal
   function GenerateRandomState: string;
   var
     RandomBytes: array[0..31] of Byte;
     I: Integer;
   begin
     // Generate 32 random bytes
     for I := 0 to 31 do
       RandomBytes[I] := Random(256);
     
     // Convert to hex string
     Result := '';
     for I := 0 to 31 do
       Result := Result + IntToHex(RandomBytes[I], 2);
   end;
   ```

2. **State Parameter**
   - Generate random state for each auth flow
   - Verify state matches on callback

3. **Token Storage Security**
   - Store in user-specific directory
   - Set file permissions to 0600
   - Consider OS keychain integration (future enhancement)

### 9. Error Handling

- Network failures during OAuth flow
- User cancellation of auth flow
- Invalid or expired tokens
- Port conflicts for local server
- Browser launch failures

### 10. Testing Strategy

1. **Unit Tests**
   - PKCE generation
   - Token storage/retrieval
   - OAuth URL building

2. **Integration Tests**
   - Full OAuth flow simulation
   - Token refresh scenarios
   - Error recovery

3. **Manual Testing**
   - Different browsers
   - Network interruptions
   - Permission denials

## Next Steps

1. ✅ **Research Complete**: Google's OAuth 2.0 implementation analyzed
2. ✅ **OAuth Credentials Confirmed**: Can use Google's public OAuth client/secret
3. **Implement Phase 1 components** using verified configuration
4. **Test with prototype** before full integration
5. **Consider fallback strategy** for environments without browser access

## Resolved Questions

1. ✅ **OAuth Support Confirmed**: Google fully supports OAuth for Gemini API access via Code Assist service
2. ✅ **Required Scopes Identified**: `cloud-platform`, `userinfo.email`, `userinfo.profile`
3. ✅ **API Endpoints Confirmed**: `https://cloudcode-pa.googleapis.com/v1internal`
4. ✅ **Authentication Pattern**: Standard Bearer token in Authorization header

## Remaining Open Questions

1. Should we support multiple auth methods (OAuth + API key) for flexibility?
2. How do we handle environments without browser access (SSH, containers)?
3. Should we implement automatic fallback to API key authentication?
4. Do we need user consent/terms acceptance for Code Assist service?

## References

### Official Google Documentation
- [OAuth 2.0 for Desktop Apps](https://developers.google.com/identity/protocols/oauth2/native-app)
- [Gemini Code Assist Documentation](https://developers.google.com/gemini-code-assist)

### Google's Official Implementation
- [gemini-cli OAuth Implementation](https://github.com/google-gemini/gemini-cli/blob/main/packages/core/src/code_assist/oauth2.ts)
- [gemini-cli API Client](https://github.com/google-gemini/gemini-cli/blob/main/packages/core/src/code_assist/server.ts)
- [google-auth-library Documentation](https://github.com/googleapis/google-auth-library-nodejs)

### OAuth Standards
- [RFC 8252: OAuth 2.0 for Native Apps](https://datatracker.ietf.org/doc/html/rfc8252)
- [OAuth 2.0 Security Best Current Practice](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-security-topics)

### Implementation Notes
- **Client Secret Handling**: Google's documentation confirms that client secrets for installed applications are not treated as secrets and can be embedded in source code
- **Token Validation**: Implementation includes both local token validation and server-side verification for enhanced security