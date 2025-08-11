# Changelog for mcp-ghcid

## [0.1.0.0] - 2025-08-11

### Added
- Initial release of MCP server for ghcid integration
- GHCID process management and monitoring
- Real-time compilation error and warning reporting
- GHCID output parsing and formatting
- MCP protocol integration for ghcid capabilities
- Support for multiple concurrent ghcid processes
- Cabal project detection and management

### Features
- Complete ghcid lifecycle management (start/stop/restart)
- Real-time stdout/stderr monitoring from ghcid processes
- Intelligent parsing of GHC error messages and warnings
- MCP tools for accessing compilation feedback
- Project-aware ghcid process spawning
- Signal handling for graceful shutdown