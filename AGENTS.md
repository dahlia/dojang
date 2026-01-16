AGENTS.md
=========

This file provides guidance to LLM-based code agents (such as OpenCode, Crush,
Claude Code, etc.) when working with code in this repository.


Project overview
----------------

Dojang is a cross-platform dotfiles manager written in Haskell. It manages
configuration files across different operating systems (Windows, macOS, Linux,
FreeBSD) by maintaining a repository of source config files and applying them
to target locations based on environment predicates.

Key concepts:

 -  *Repository*: Directory containing source config files and a manifest
 -  *Manifest* (*dojang.toml*): TOML file declaring how config files should be
    applied
 -  *Monikers*: Named environment criteria (e.g., `windows`, `posix`,
    `apple_silicon_mac`)
 -  *Environment predicates*: Conditions for matching environments (OS, arch,
    kernel)
 -  *File routes*: Mappings from source files to target paths based on predicates
 -  *Application*: Syncing source files to target locations
 -  *Reflection*: Syncing modified target files back to source


Build & test commands
---------------------

### Building

~~~~ bash
# Standard build
stack build

# Build with static linking (Linux)
stack build --flag dojang:static

# Build with optimization
stack build --ghc-options=-O2

# Build and install to local bin path
stack build --copy-bins --local-bin-path=./bin/
~~~~

### Testing

~~~~ bash
# Run all tests
stack test

# Run tests with coverage
stack test --coverage

# Run a specific test module (use the module path from test/)
stack test --test-arguments="--match \"Dojang.Types.FileRoute\""

# Run with JUnit output (for CI)
JUNIT_ENABLED=1 JUNIT_OUTPUT_DIRECTORY=.test_report JUNIT_SUITE_NAME=dojang-spec stack test
~~~~

### Formatting & linting

~~~~ bash
# Format Haskell code (uses fourmolu.yaml configuration)
fourmolu -i src/ app/ test/

# Check if formatting is correct
fourmolu --mode check src/ app/ test/
~~~~

### Documentation

~~~~ bash
# Build documentation site (requires mkdocs and dependencies)
pip install -r doc/requirements.txt
mkdocs build

# Serve documentation locally
mkdocs serve
~~~~

### Verification

~~~~ bash
# Check documentation contents
python3 scripts/check-doc-contents.py doc
~~~~


Architecture
------------

### Core type system

The codebase is built around several key types that work together:

1.  *Environment* (`Dojang.Types.Environment`): Represents the current system
    environment (OS, architecture, kernel info)

2.  *Environment predicate* (`Dojang.Types.EnvironmentPredicate`): Boolean
    expressions for matching environments

     -  Supports: `Always`, `Not`, `And`, `Or`, `Moniker`, `OperatingSystem`,
        `Architecture`, `KernelName`, `KernelRelease`, etc.
     -  Includes normalization logic to simplify predicates

3.  *File route* (`Dojang.Types.FileRoute`): Maps predicates to file path
    expressions

     -  Uses specificity ordering to determine which route matches first
     -  Supports “null routes” (predicates that explicitly ignore files)

4.  *Manifest* (`Dojang.Types.Manifest`): Contains monikers, file routes, and
    ignore patterns

     -  Loaded from *dojang.toml* using TOML parser

5.  *Repository* (`Dojang.Types.Repository`): Represents a dotfiles repository
    with its manifest

6.  *Context* (`Dojang.Types.Context`): Combines repository, environment, and
    variable lookup

### Monad stack

The application uses a custom monad stack:

~~~~ haskell
App i v = ReaderT AppEnv (LoggingT i) v
~~~~

Where:

 -  `AppEnv` contains source directory, manifest file path, dry-run flag, debug flag
 -  `LoggingT` provides logging capabilities
 -  `i` is the inner monad (usually `IO` or a test monad)
 -  Implements `MonadFileSystem` for abstracting file operations (enables testing)

### Command structure

Commands are in `Dojang.Commands.*`:

 -  `Apply`: Apply source files to target locations
 -  `Reflect`: Sync target files back to source
 -  `Diff`: Show differences between source and target
 -  `Status`: Show current state
 -  `Init`: Initialize a new repository
 -  `Env`: Display current environment

Each command follows the pattern:

1.  Load repository and environment via `ensureContext`
2.  Evaluate file routes based on environment
3.  Perform file operations (with dry-run support)
4.  Report results with colored, friendly error messages

### File path expression system

File paths can contain expressions that are expanded at runtime:

 -  Environment variables: `$HOME`, `%USERPROFILE%`
 -  Tilde expansion: `~/` or `~\`
 -  Cross-platform path handling via `System.OsPath`

### Parser infrastructure

Uses Megaparsec for parsing:

 -  Manifest parser (`Dojang.Syntax.Manifest.Parser`): Parses dojang.toml
 -  Environment predicate parser (`Dojang.Syntax.EnvironmentPredicate.Parser`):
    Parses `when` expressions
 -  File path expression parser (`Dojang.Syntax.FilePathExpression.Parser`)
 -  Env file parser (`Dojang.Syntax.Env`): Parses .dojang.env files

All parsers return detailed error messages with position information.

### Testing strategy

Tests use Hspec with:

 -  Property-based testing via Hedgehog (`Dojang.Gen`, `Dojang.Types.Gen`)
 -  In-memory file system for testing file operations (`MonadFileSystem`
    abstraction)
 -  Megaparsec testing utilities (`hspec-megaparsec`)
 -  Pretty diff output (`hspec-expectations-pretty-diff`)

Test organization mirrors src/ structure.


Important conventions
---------------------

### Error handling

 -  Use `ExitCode` constants from `Dojang.ExitCodes` (never hardcode exit codes)
 -  Provide friendly error messages via `Dojang.Commands` utilities
 -  Include hints and warnings using `Admonition` types

### File system operations

 -  Always use `MonadFileSystem` abstraction, never direct IO
 -  Use `System.OsPath` for cross-platform path handling
 -  Normalize paths with `normalise` before comparison

### Logging

 -  Use Template Haskell logging: `$(logDebug)`, `$(logInfo)`, `$(logWarn)`,
    `$(logError)`
 -  Include `FromStringShow` wrappers for showing complex types in logs

### Code style

 -  Follow *fourmolu.yaml* configuration (2-space indentation, 80-column limit)
 -  Use record dot syntax for field access (`OverloadedRecordDot`)
 -  Disable field selectors (`NoFieldSelectors`) and use HasField/record updates
 -  Include comprehensive Haddock documentation for exported functions

### Cross-platform support

 -  Test on Windows, macOS, Linux (Ubuntu), and FreeBSD (Cirrus CI)
 -  Handle platform-specific path separators
 -  Support both POSIX and Windows environment conventions


Package management
------------------

This project uses Stack (not Cabal directly):

 -  *package.yaml*: Hpack configuration (generates .cabal file)
 -  *stack.yaml*: Main Stack configuration (LTS 21.21, GHC 9.4.8)
 -  *stack-ghc-9.4.5.yaml*: Alternative Stack config for FreeBSD

Dependencies are declared in *package.yaml* with version bounds.
