# Demo Project

This is a comprehensive demo project designed for testing HLS (Haskell Language Server) tools and MCP (Model Context Protocol) integration.

## Modules

- **Demo.Basic**: Basic functions for testing hover info, goto definition, and find references
- **Demo.Diagnostics**: Intentional errors, warnings, and missing signatures for diagnostics testing
- **Demo.Actions**: Functions without type signatures and refactoring targets for code actions
- **Demo.Imports**: Complex import scenarios for import management testing
- **Demo.Advanced**: GADTs, Template Haskell, and advanced type features
- **Demo.Completions**: Various scenarios for testing code completions
- **Demo.Formatting**: Poorly formatted code for testing formatting tools
- **Demo.CodeLens**: Functions that should generate helpful code lenses
- **Demo.Evaluation**: Expressions designed for eval_expression testing

## Usage

This project is designed to be used with HLS for comprehensive tool testing. Each module targets specific HLS features and MCP tools.

## Building

```bash
cabal build
cabal test
cabal bench
```