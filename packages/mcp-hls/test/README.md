# HLS Integration Test Suite

This test suite validates that we can successfully communicate with `haskell-language-server-wrapper` using the LSP protocol.

## Prerequisites

- `haskell-language-server-wrapper` must be available on your PATH
- Install using: `ghcup install hls`, `stack install haskell-language-server`, or `cabal install haskell-language-server`

## Running Tests

```bash
# Run the test suite
cabal test

# Run with verbose output
cabal test --test-show-details=direct

# Run only the integration tests
cabal test hls-integration-tests
```

## Test Coverage

The test suite covers:

1. **Process Management**
   - Starting and stopping HLS processes
   - Proper initialization and shutdown

2. **LSP Protocol**
   - Message formatting and parsing
   - Request/response handling
   - Notification handling

3. **Text Document Operations**
   - Opening documents
   - Hover information requests
   - Diagnostic reporting

4. **Workspace Operations**
   - Symbol queries
   - Workspace analysis

## Test Structure

- `Test.Utils` - Utilities for HLS process management and LSP communication
- `Test.Fixtures` - Sample Haskell code and project files for testing
- `HLS.IntegrationSpec` - Main test specifications
- `fixtures/` - Static test files

## Debugging Failed Tests

If tests fail:

1. Ensure HLS is installed and working: `haskell-language-server-wrapper --version`
2. Check that HLS can analyze Haskell files in your project
3. Review test output for specific error messages
4. Tests may be sensitive to timing - HLS needs time to analyze files