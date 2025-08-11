# Haskell Language Server (HLS) Commands

This document lists the custom commands supported by the Haskell Language Server and its plugins. These commands can be invoked via the `workspace/executeCommand` LSP request. For each command, the command ID and the expected JSON payload structure are provided.

---

## `hls-cabal-plugin`

This plugin provides commands for interacting with Cabal files.

### `cabalAddDependency`

- **Command ID:** `cabalAddDependency`
- **Description:** Adds a new package dependency to a `.cabal` file.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-cabal-plugin/src/Ide/Plugin/Cabal/CabalAdd/Types.hs` (`CabalAddDependencyCommandParams`)
- **JSON Payload:**
  ```json
  {
    "depCabalPath": "string (filepath)",
    "depVerTxtDocId": {
      "uri": "string (uri)",
      "version": "integer"
    },
    "depBuildTarget": "string (optional)",
    "depDependency": "string",
    "depVersion": "string (optional)"
  }
  ```

### `cabalAddModule`

- **Command ID:** `cabalAddModule`
- **Description:** Adds a new module to a specific component in a `.cabal` file.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-cabal-plugin/src/Ide/Plugin/Cabal/CabalAdd/Types.hs` (`ModuleInsertionConfig`)
- **JSON Payload:**
  ```json
  {
    "targetFile": "string (filepath)",
    "moduleToInsert": "string",
    "modVerTxtDocId": {
      "uri": "string (uri)",
      "version": "integer"
    },
    "insertionStanza": "string (ComponentName)",
    "insertionLabel": "string"
  }
  ```

---

## `hls-class-plugin`

This plugin provides commands for working with class instances.

### `classplugin.typelens`

- **Command ID:** `classplugin.typelens`
- **Description:** Adds a type signature for a method in a class instance (typically invoked from a code lens).
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-class-plugin/src/Ide/Plugin/Class/Types.hs` (`InstanceBindLensCommand`)
- **JSON Payload:**
  ```json
  {
    "commandUri": "string (uri)",
    "commandEdit": {
      "range": {
        "start": { "line": "integer", "character": "integer" },
        "end": { "line": "integer", "character": "integer" }
      },
      "newText": "string"
    }
  }
  ```

### `classplugin.codeaction`

- **Command ID:** `classplugin.codeaction`
- **Description:** Adds placeholders for the minimal methods required by a class instance.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-class-plugin/src/Ide/Plugin/Class/Types.hs` (`AddMinimalMethodsParams`)
- **JSON Payload:**
  ```json
  {
    "verTxtDocId": {
      "uri": "string (uri)",
      "version": "integer"
    },
    "range": {
      "start": { "line": "integer", "character": "integer" },
      "end": { "line": "integer", "character": "integer" }
    },
    "methodGroup": [["string", "string"]],
    "withSig": "boolean"
  }
  ```

---

## `hls-eval-plugin`

This plugin evaluates code snippets within comments.

### `evalCommand`

- **Command ID:** `evalCommand`
- **Description:** Evaluates a section of code within a file.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Types.hs` (`EvalParams`)
- **JSON Payload:**
  ```json
  {
    "sections": [
      {
        "sectionName": "string",
        "sectionTests": [
          {
            "testLines": ["string"],
            "testOutput": ["string"],
            "testRange": {
              "start": { "line": "integer", "character": "integer" },
              "end": { "line": "integer", "character": "integer" }
            }
          }
        ],
        "sectionLanguage": "string (Plain|Haddock)",
        "sectionFormat": {
          "tag": "string (SingleLine|MultiLine)",
          "contents": {
            "start": { "line": "integer", "character": "integer" },
            "end": { "line": "integer", "character": "integer" }
          }
        }
      }
    ],
    "module_": {
      "uri": "string (uri)"
    },
    "evalId": "integer"
  }
  ```

---

## `hls-explicit-imports-plugin`

This plugin provides commands for managing explicit import lists.

### `ImportLensCommand`

- **Command ID:** `ImportLensCommand`
- **Description:** Refines an import statement, typically to make it explicit.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-explicit-imports-plugin/src/Ide/Plugin/ExplicitImports.hs` (`IAResolveData`)
- **JSON Payload:**
  ```json
  {
    "tag": "string (ResolveOne|ExplicitAll|RefineAll)",
    "uri": "string (uri)",
    "importId": "integer (optional, required for ResolveOne)"
  }
  ```

---

## `hls-gadt-plugin`

This plugin provides commands for converting data declarations to GADT syntax.

### `GADT.toGADT`

- **Command ID:** `GADT.toGADT`
- **Description:** Converts a Haskell 98-style data declaration to GADT syntax.
- **Payload Source:** `mcp-hls/haskell-language-server/plugins/hls-gadt-plugin/src/Ide/Plugin/GADT.hs` (`ToGADTParams`)
- **JSON Payload:**
  ```json
  {
    "uri": "string (uri)",
    "range": {
      "start": { "line": "integer", "character": "integer" },
      "end": { "line": "integer", "character": "integer" }
    }
  }
  ```

---

## `ghcide` / `hls-refactor-plugin`

The core `ghcide` engine provides refactoring and completion commands.

### `hie.completion.extendImport`

- **Command ID:** `hie.completion.extendImport`
- **Description:** Extends an existing import list to include a new symbol, typically used after a code completion.
- **Payload Source:** `mcp-hls/haskell-language-server/ghcide/src/Development/IDE/Plugin/Completions/Types.hs` (`ExtendImport`)
- **JSON Payload:**
  ```json
  {
    "doc": "string (uri)",
    "newThing": "string",
    "thingParent": "string (optional)",
    "importName": "string",
    "importQual": "string (optional)"
  }
  ```
