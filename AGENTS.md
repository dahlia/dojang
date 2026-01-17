AGENTS.md
=========

This file provides guidance to LLM-based code agents (such as OpenCode, Crush,
Claude Code, etc.) when working with code in this repository.


Project overview
----------------

Dojang is a cross-platform dotfiles manager written in Haskell. It manages
configuration files across different operating systems (Windows, macOS, Linux)
by maintaining a repository of source config files and applying them to target
locations based on environment predicates.

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


Development practices
---------------------

### Test-driven development

This project strictly follows test-driven development (TDD) practices.
All new code must be developed using the TDD cycle:

1.  *Red*: Write a failing test that describes the expected behavior.
    Run the test to confirm it fails.
2.  *Green*: Write the minimum code necessary to make the test pass.
    Run the test to confirm it passes.
3.  *Refactor*: Improve the code while keeping all tests passing.
    Run tests after each refactoring step.

Additional TDD guidelines:

 -  *Write tests first*: Before implementing new functionality, write tests
    that describe the expected behavior.  Confirm that the tests fail before
    proceeding with the implementation.
 -  *Regression tests for bugs*: When fixing bugs, first write a regression
    test that reproduces the bug.  Confirm that the test fails, then fix the
    bug and verify the test passes.
 -  *Small increments*: Implement features in small, testable increments.
    Each increment should have its own test.
 -  *Run tests frequently*: Run `stack test` after every change to ensure
    existing functionality is not broken.

### Before committing

 -  *Run all tests*: Before committing any changes, run `stack test` to
    ensure all tests pass.
 -  *Check formatting*: Run `fourmolu --mode check src/ app/ test/` to
    verify code formatting.
 -  *Build successfully*: Ensure `stack build` completes without errors
    or warnings.

### Commit messages

 -  Do not use Conventional Commits (no `fix:`, `feat:`, etc. prefixes).
    Keep the first line under 50 characters when possible.

 -  Focus on *why* the change was made, not just *what* changed.

 -  When referencing issues or PRs, use permalink URLs instead of just
    numbers (e.g., `#123`).  This preserves context if the repository
    is moved later.

 -  When listing items after a colon, add a blank line after the colon:

    ~~~~
    This commit includes the following changes:

    - Added foo
    - Fixed bar
    ~~~~

 -  When using LLMs or coding agents, include credit via `Co-Authored-By:`.
    Include a permalink to the agent session if available.

### Changelog (*CHANGES.md*)

This repository uses *CHANGES.md* as a human-readable changelog.  Follow these
conventions:

 -  *Structure*: Keep entries in reverse chronological order (newest version at
    the top).

 -  *Version sections*: Each release is a top-level section:

    ~~~~
    Version 0.1.0
    -------------
    ~~~~

 -  *Unreleased version*: The next version should start with:

    ~~~~
    To be released.
    ~~~~

 -  *Released versions*: Use a release-date line right after the version header:

    ~~~~
    Released on December 30, 2025.
    ~~~~

    If you need to add brief context (e.g., initial release), keep it on the
    same sentence:

    ~~~~
    Released on August 21, 2025.  Initial release.
    ~~~~

 -  *Bullets and wrapping*: Use ` -  ` list items, wrap around ~80 columns, and
    indent continuation lines by 4 spaces so they align with the bullet text.

 -  *Write useful change notes*: Prefer concrete, user-facing descriptions.
    Include what changed, why it changed, and what users should do differently
    (especially for breaking changes, deprecations, and security fixes).

 -  *Multi-paragraph items*: For longer explanations, keep paragraphs inside the
    same bullet item by indenting them by 4 spaces and separating paragraphs
    with a blank line (also indented).

 -  *Code blocks in bullets*: If a bullet includes code, indent the entire code
    fence by 4 spaces so it remains part of that list item.  Use `~~~~` fences
    and specify a language (e.g., `~~~~ haskell`).

 -  *Nested lists*: If you need sub-items (e.g., a list of added features), use
    a nested list inside the parent bullet, indented by 4 spaces.

 -  *Issue and PR references*: Use `[[#123]]` markers in the text and add
    reference links at the end of the version section.

    When listing multiple issues/PRs, list them like `[[#123], [#124]]`.

    When the reference is for a PR authored by an external contributor, append
    `by <NAME>` after the last reference marker (e.g., `[[#123] by Hong Minhee]`
    or `[[#123], [#124] by Hong Minhee]`).

    ~~~~
    [#123]: https://github.com/dahlia/dojang/issues/123
    [#124]: https://github.com/dahlia/dojang/pull/124
    ~~~~


Important conventions
---------------------

### Error handling

 -  Use `ExitCode` constants from `Dojang.ExitCodes` (never hardcode exit codes)
 -  Provide friendly error messages via `Dojang.Commands` utilities
 -  Include hints and warnings using `Admonition` types
 -  End error messages with a period

### File system operations

 -  Always use `MonadFileSystem` abstraction, never direct IO
 -  Use `System.OsPath` for cross-platform path handling
 -  Normalize paths with `normalise` before comparison

### Logging

 -  Use Template Haskell logging: `$(logDebug)`, `$(logInfo)`, `$(logWarn)`,
    `$(logError)`
 -  Include `FromStringShow` wrappers for showing complex types in logs
 -  End log messages with a period, or with an ellipsis (`...`) for ongoing
    operations

### Code style

 -  Follow *fourmolu.yaml* configuration (2-space indentation, 80-column limit)
 -  Use record dot syntax for field access (`OverloadedRecordDot`)
 -  Disable field selectors (`NoFieldSelectors`) and use HasField/record updates
 -  Include comprehensive Haddock documentation for exported functions
 -  All exported APIs must have Haddock comments describing their purpose,
    parameters, and return values

### Type safety

 -  All code must be type-safe.  Avoid using unsafe operations unless
    absolutely necessary.
 -  Prefer immutable data structures unless there is a specific reason to
    use mutable ones.

### Cross-platform support

 -  Test on Windows, macOS, and Linux (Ubuntu)
 -  Handle platform-specific path separators
 -  Support both POSIX and Windows environment conventions


Package management
------------------

This project uses Stack (not Cabal directly):

 -  *package.yaml*: Hpack configuration (generates .cabal file)
 -  *stack.yaml*: Main Stack configuration (LTS 24.28, GHC 9.10.3)

Dependencies are declared in *package.yaml* with version bounds.


Markdown style guide
--------------------

When creating or editing Markdown documentation files in this project,
follow these style conventions to maintain consistency with existing
documentation:

### Headings

 -  *Setext-style headings*: Use underline-style for the document title
    (with `=`) and sections (with `-`):

    ~~~~
    Document Title
    ==============

    Section Name
    ------------
    ~~~~

 -  *ATX-style headings*: Use only for subsections within a section:

    ~~~~
    ### Subsection Name
    ~~~~

 -  *Heading case*: Use sentence case (capitalize only the first word and
    proper nouns) rather than Title Case:

    ~~~~
    Development practices    ← Correct
    Development Practices    ← Incorrect
    ~~~~

### Text formatting

 -  *Italics* (`*text*`): Use for package names (*dojang*, *stack*),
    emphasis, and to distinguish concepts
 -  *Bold* (`**text**`): Use sparingly for strong emphasis
 -  *Inline code* (`` `code` ``): Use for code spans, function names,
    module names, filenames, and command-line options

### Lists

 -  Use ` -  ` (space-hyphen-two spaces) for unordered list items

 -  Indent nested items with 4 spaces

 -  Align continuation text with the item content:

    ~~~~
     -  *First item*: Description text that continues
        on the next line with proper alignment
     -  *Second item*: Another item
    ~~~~

### Code blocks

 -  Use four tildes (`~~~~`) for code fences instead of backticks

 -  Always specify the language identifier:

    ~~~~~
    ~~~~ haskell
    exampleFunction :: Int -> String
    exampleFunction n = show n
    ~~~~
    ~~~~~

 -  For shell commands, use `bash`:

    ~~~~~
    ~~~~ bash
    stack test
    ~~~~
    ~~~~~

### Links

 -  Use reference-style links placed at the *end of each section*
    (not at document end)

 -  Format reference links with consistent spacing:

    ~~~~
    See the [Haskell Wiki] for more information.

    [Haskell Wiki]: https://wiki.haskell.org/
    ~~~~

### Spacing and line length

 -  Wrap lines at approximately 80 characters for readability
 -  Use one blank line between sections and major elements
 -  Use two blank lines before Setext-style section headings
 -  Place one blank line before and after code blocks
