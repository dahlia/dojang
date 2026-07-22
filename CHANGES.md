Dojang changelog
================

Version 0.3.0
-------------

To be released.

### Command-line interface

 -  Command execution now routes environment reads, platform detection, clocks,
    random identifiers, terminal checks, prompts, output, and child processes
    through explicit interpreters.  Dry runs still accept interactive input but
    never start diff tools, editors, hooks, or other external programs.  Hook
    processes are started while their repository state generation is validated,
    then awaited without holding the repository state lock so an explicitly
    allowed nested Dojang invocation cannot deadlock.  [[#46], [#72]]

 -  Detailed route branches can declare a byte-oriented `codec`.  The default
    `identity` codec preserves all existing behavior.  Applying a route writes
    its rendered bytes through the intermediate snapshot, while status, diff,
    edit, and reflection use the same rendered source view.  Registered codecs
    declare their inputs, dry-run behavior,
    cache scope, and `identity`, `reject`, or validated `re-add` reflection
    policy.  `--force` cannot bypass a codec's reflection policy.  Codec
    diagnostics and plans redact source and rendered contents.  The built-in
    diff reports non-UTF-8 rendered output as binary.  File routes using a
    non-identity codec reject directory or symbolic-link destinations before
    changing repository files.  Codec registration and configuration are
    validated even when the selected source is missing, before a forced
    operation can delete its destination.  Before writing a buffered codec
    result, Dojang verifies that its authoritative input has not changed since
    evaluation.  Cache-only dry runs reject codecs with external inputs before
    invoking their resolver.  Codec failures use exit code 39.  [[#43], [#70]]

 -  Added the built-in, deterministic `template` codec.  UTF-8 repository
    sources can use a pure Ginger/Jinja subset with `vars` for active manifest
    variables and case-insensitive `facts` for machine facts.  Template
    variables never fall back to the process environment.  Input dependencies
    are derived from each source so unrelated values do not invalidate its
    persistent cache; missing values fail only along the branch that executes.
    Diagnostics retain the missing input namespace and include source line and
    column while redacting contents; runtime-derived lookup keys are reported
    as `<dynamic>`, and invalid platform text identifies only the affected
    variable name.  Partial Ginger arithmetic, formatting, and
    text-transformation operations are rejected before evaluation instead of
    terminating the process.  Local bindings are validated in execution order
    and across every possible branch, and every valid Ginger script opener,
    including comment-prefixed forms, is rejected.  Template reflection is
    rejected even with `--force`.  [[#44], [#71]]

 -  Machine-state schema version 6 records only redacted codec metadata: the
    implementation name and version, configuration digest, cache key, and
    dependency fingerprints.  Rendered bytes remain in the intermediate
    snapshot instead of being duplicated in state.  A cached result is reused
    only when its deterministic key and recorded file fingerprint still match.
    Schema versions 1 through 5 remain readable.  [[#43], [#70]]

 -  Detailed `[files]` and `[dirs]` route branches can declare portable
    destination modes.  The available modes are `private` (files `0600`,
    directories `0700`), `executable` (`0755`),
    `private-executable` (`0700`), and `read-only` (files `0444`,
    directories `0555`).  `dojang status` reports mode-only changes, while
    `dojang apply` and `dojang reflect` restore the declared mode even when
    the contents have not changed.  Windows enforces only `read-only` and
    warns about other modes.  Compact routes remain valid.  Executable modes
    cannot be used for directory routes.  [[#33], [#42], [#69]]

 -  A detailed route can set `kind = "symlink"` to deploy a symbolic link
    whose target is the absolute path of the source in the repository.
    `dojang apply` creates missing links and repairs links after the checkout
    moves.  It also recognizes a broken link whose target is already correct.
    An existing regular entry is a conflict unless `--force` is used.
    `dojang reflect` refuses deployment links, even with `--force`, and never
    follows a linked directory.  Deployment links cannot declare a mode.  On
    Windows, creating one requires Developer Mode or administrator privileges.
    [[#33], [#42], [#69]]

 -  Each destination entry is now owned by the route with the most specific
    destination.  A broader directory route preserves entries owned by nested
    routes, and a deployed directory link forms a traversal boundary.
    Duplicate or aliased destinations, routes through deployment links,
    destinations that overlap the repository, and destinations that alias
    their own source are rejected before any files are changed.  These
    configuration errors use exit code 38.  [[#33], [#42], [#69]]

 -  Added reusable, case-sensitive manifest variables in the `[vars]` table.
    Values can be unconditional or selected by a moniker or environment
    predicate.  Variables may reference one another in any declaration order,
    override inherited environment variables, and preserve an explicitly
    selected empty value.  If no branch matches, lookup falls back to the
    inherited value.  Reference cycles are reported before synchronization.
    Machine facts can select a variable branch but are not exposed as variable
    values.  Manifest variables are configuration, not a place to store
    secrets.  [[#33], [#40], [#68]]

 -  File routes, directory routes, and hook `working-directory` fields now use
    the same path-expression lookup.  Relative hook working directories remain
    repository-relative.  A literal dollar sign in a path expression must now
    be written as `$$`; existing hook working directories containing `$`
    must be updated.  Values used by an `on-change` hook affect its fingerprint
    without being stored as plaintext.  [[#33], [#40], [#68]]

 -  Manifests can distinguish machines with custom facts and the
    `fact.KEY` predicate syntax.  Dojang also detects a `hostname` fact.
    Fact names and values are case-insensitive, while the existing
    operating-system, architecture, and kernel predicates retain their
    behavior and specificity.  Undefined facts warn and do not match, and
    declared facts cannot replace detected built-in facts.  [[#33], [#41],
    [#67]]

 -  Put reusable, nonsecret machine values in the `[facts]` table.  Running
    `dojang init` in an existing repository now enrolls the current machine
    and prompts for required facts.  Noninteractive use can supply repeatable
    `--fact KEY=VALUE` options or reuse a TOML profile with
    `--facts-file PATH`.  Repository-specific values override the shared
    profile, while `-e`/`--env-file` remains a separate, higher-precedence
    simulation layer.  Missing required values use exit code 22.
    `dojang forget` removes the repository's declared facts and profile
    association.  Dry runs do not save enrollment.  Facts may appear in
    diagnostics, state, and fingerprints, so they must not contain credentials.
    [[#33], [#41], [#67]]

 -  Machine-state schema version 5 records each route's declared kind and
    mode, using a deployment link's target as its fingerprint.  Machine facts
    and shared-profile associations are also stored with the repository's
    state.  Schema versions 1 through 4 remain readable.  [[#33], [#40],
    [#41], [#42], [#67], [#68], [#69]]

 -  Hooks can now run around the `apply`, `reflect`, `diff`, `status`,
    `edit`, and `unmanage` commands.  Post-hooks run only after a successful
    command.  Stable hook IDs support `once` and explicitly keyed
    `on-change` policies, with successful runs tracked for each repository
    and machine.  Dry runs explain which hooks would run without starting
    them or updating history.  Failed ignored hooks remain eligible for the
    next run.  Hook processes receive the command, lifecycle event, repository,
    machine-state, platform, and selected-path context.  [[#34], [#39], [#66]]

 -  Nested Dojang commands suppress hooks by default.
    `--allow-hook-recursion` permits one nested level but prevents a hook from
    re-entering itself in the same repository.  Stateful hook runs are
    serialized, and forgetting then recreating a repository cannot let an old
    waiting or completed hook update the new state.  [[#34], [#39], [#66]]

 -  Successful `dojang apply` and `dojang reflect` operations now record
    each managed destination and its last synchronized state.  Partial failures
    update only entries that have already converged, and dry runs do not change
    these records.  Route names that escape the source tree or normalize to the
    same path are rejected before synchronization.  Path identity follows the
    host platform, including case-insensitive destination matching on Windows.
    [[#34], [#38], [#65]]

 -  `dojang status` reports orphaned destinations when a route is removed,
    stops matching, changes definition, moves to another destination, or no
    longer produces a previously managed directory entry.  It distinguishes
    unchanged, modified, and missing orphans without deleting them.  Named
    pipes, sockets, devices, and other special entries are reported as modified
    without being read.  [[#34], [#38], [#65]]

 -  Added `dojang unmanage --route ROUTE [DESTINATION ...]` to discard orphan
    records and their unused snapshots without removing destination files.
    Modified destinations require `--force`.  `dojang forget` removes only
    the selected repository's machine-local records, snapshots, and first-apply
    history; it leaves source and destination files alone.  Interrupted cleanup
    can be retried, and other stateful commands wait until that cleanup is
    complete.  [[#34], [#38], [#65]]

 -  Added versioned, repository-scoped machine state under the platform's user
    data directory.  New repositories receive a stable `repository-id`, and
    intermediate snapshots now default outside the worktree.  Existing
    repositories must run `dojang migrate`, which copies and verifies the old
    *.dojang/* snapshot before removing it and can resume an interrupted
    migration.  `--dry-run` does not save migration work.  Existing custom
    snapshots can be migrated in place with `--intermediate-dir`.
    [[#34], [#37], [#64]]

 -  Machine-state migration rejects snapshots that are missing, unowned,
    malformed, symlinked, or overlap protected paths.  State and manifest
    replacement preserve native paths and POSIX modes, and corrupt or newer
    state files produce specific repair or upgrade guidance.  Failed copy and
    cleanup work is journaled so that a retry cannot delete an unrelated or
    divergent directory.  [[#34], [#37], [#64]]

 -  The old *~/.dojang* file no longer selects one global current repository.
    Dojang selects a repository automatically only when exactly one valid
    machine-state record points to its current checkout.  Missing, reused, or
    ambiguous checkouts require `-r`/`--repository-dir` instead of falling
    back to another directory.  `dojang init` and `dojang migrate` remain
    anchored to the current or explicitly selected directory, while
    `dojang env --ignore-env-file` does not select a repository.  First-apply
    history is tracked separately for each repository.  [[#34], [#37], [#64]]

 -  Added detailed file and directory routes with ordered `moniker` or inline
    `when` conditions.  Compact routes remain valid.  Detailed routes can
    preserve repeated conditions, direct environment predicates, and null
    routes that the compact form cannot express without changing their
    meaning.  [[#32], [#63]]

 -  `dojang apply` and `dojang reflect` now inspect conflicts and
    destructive operations before executing one ordered reconciliation plan.
    A source-only change no longer blocks `dojang reflect`; only incompatible
    changes on both sides are conflicts.  [[#28], [#31], [#62]]

 -  File and directory routes now use the same state observation and change
    calculation.  Dojang detects symbolic links without following them,
    reports removed entries consistently, and compares equal-size regular
    files by content.  [[#28], [#30], [#60]]

[#28]: https://github.com/dahlia/dojang/issues/28
[#30]: https://github.com/dahlia/dojang/issues/30
[#31]: https://github.com/dahlia/dojang/issues/31
[#32]: https://github.com/dahlia/dojang/issues/32
[#33]: https://github.com/dahlia/dojang/issues/33
[#34]: https://github.com/dahlia/dojang/issues/34
[#37]: https://github.com/dahlia/dojang/issues/37
[#38]: https://github.com/dahlia/dojang/issues/38
[#39]: https://github.com/dahlia/dojang/issues/39
[#40]: https://github.com/dahlia/dojang/issues/40
[#41]: https://github.com/dahlia/dojang/issues/41
[#42]: https://github.com/dahlia/dojang/issues/42
[#43]: https://github.com/dahlia/dojang/issues/43
[#44]: https://github.com/dahlia/dojang/issues/44
[#46]: https://github.com/dahlia/dojang/issues/46
[#60]: https://github.com/dahlia/dojang/pull/60
[#62]: https://github.com/dahlia/dojang/pull/62
[#63]: https://github.com/dahlia/dojang/pull/63
[#64]: https://github.com/dahlia/dojang/pull/64
[#65]: https://github.com/dahlia/dojang/pull/65
[#66]: https://github.com/dahlia/dojang/pull/66
[#67]: https://github.com/dahlia/dojang/pull/67
[#68]: https://github.com/dahlia/dojang/pull/68
[#69]: https://github.com/dahlia/dojang/pull/69
[#70]: https://github.com/dahlia/dojang/pull/70
[#71]: https://github.com/dahlia/dojang/pull/71
[#72]: https://github.com/dahlia/dojang/pull/72

### Haskell API

 -  Added `MonadCommandEffect`, `MonadProcessControl`, structured process and
    prompt requests, production and dry-run interpreters, and an exact scripted
    test interpreter.  `App` delegates command effects to its selected
    interpreter, so embedding tests can replay complete commands without using
    host environment, clock, terminal, prompt, output, or process APIs.  The
    scripted interpreter records process starts, waits, and cancellations
    separately, and rejects leaked processes.  `App` no longer has a `MonadIO`
    instance: embedding code should add a dedicated effect, or use `liftApp`
    only at an interpreter or test boundary.  Command exits now travel through
    `ExceptT ExitCode`; use `runAppResultWithoutLogging`,
    `runAppResultWithLogging`, or `runAppResultWithStderrLogging` to receive
    them as values.  The older runners remain as compatibility adapters that
    throw the returned exit code.  [[#46], [#72]]

 -  Added declarative codec types, deterministic cache keys, a controlled
    effect interpreter, typed redacted failures, and explicit runtime injection
    for apply, status, diff, edit, and reflect commands.  `CodecImplementation`
    transformations are now pure rather than monadic; embedding applications
    must move filesystem, process, or secret lookup effects into the declared
    external-input resolver.  Command-scoped evaluations are reused while the
    raw source remains stable.  Forward and reverse transformations receive the
    validated route configuration.  Their resolved inputs are frozen for
    re-add reflection, and manifest variables reach codecs as native
    operating-system bytes rather than a lossy `Text` conversion.  Convergence
    publication rechecks the raw source.  `CodecEvaluationRequest.variables`
    therefore now uses `Map Text ByteString`.  [[#43], [#70]]

 -  Added source-derived codec requirements and structured, source-positioned
    codec failures, plus `builtInCodecRuntime` and the built-in template codec
    specification and definition.  The positional `CodecImplementation`
    constructor keeps its six arguments, while callbacks now return
    `CodecFailure` and `CodecRequirements` uses `CodecInputSelection`; replace
    input sets with `requiredCodecInputs` or `noCodecInputs`.  Embeddings can
    opt into source analysis with `codecImplementationWithSourceRequirements`.
    `codecRequirements` now also accepts the raw source bytes. [[#44], [#71]]

 -  `FileRoute` branches now carry a `RouteTarget`, which combines a path
    expression with `RouteMode` and `RouteKind`, instead of a bare
    `FilePathExpression`.  `RouteResult` and `ManagedTarget` expose the
    selected metadata.  `MonadFileSystem` can observe portable modes and
    create symbolic links, and reconciliation plans include explicit mode,
    link, and ownership-preserving removal operations.  [[#33], [#42], [#69]]

 -  `Manifest` now has a `variables` field, and `Hook.workingDirectory`
    changed from `Maybe OsPath` to `Maybe FilePathExpression`.  The existing
    `manifest` helper still creates a manifest with no variables; use
    `manifestWithVariables` to supply them.  `Context.variableGetter` now
    reports provenance and warnings.  [[#33], [#40], [#68]]

 -  `Environment` now stores custom facts in `additionalFacts`.  Code using
    the old three-argument constructor must call `emptyEnvironment` or
    construct the four-field record.  Record updates preserve custom facts.
    This source-incompatible change does not alter the CLI, manifest,
    environment-file, or machine-state formats.  [[#33], [#41], [#67]]

 -  Manifest serialization now rejects invalid hook policy combinations and
    duplicate stateful IDs instead of writing a manifest that the parser would
    reject.  Validated hook IDs remain abstract in the public API.  [[#34],
    [#39], [#66]]

 -  Manifest writing is total for detailed routes and uses the detailed form
    whenever the compact form would change condition semantics, priority, or
    multiplicity.  [[#32], [#63]]

 -  Added a pure, direction-aware reconciliation planner and a shared
    filesystem operation executor.  Plans expose conflicts, skipped entries,
    and destructive operations before execution.  [[#28], [#29], [#61]]

[#29]: https://github.com/dahlia/dojang/issues/29
[#61]: https://github.com/dahlia/dojang/pull/61


Version 0.2.1
-------------

Released on July 12, 2026.

 -  Fixed a bug where `dojang reflect` crashed after deleting the intermediate
    copy when a managed file had been removed at its destination.
    Destination-side deletions are now reflected to the source and can also be
    selected by naming the deleted destination path explicitly.  [[#24]]

 -  Fixed hook commands discarding the parent process environment.  Hooks now
    inherit variables such as `PATH` and `HOME`, while the `DOJANG_*` variables
    supplied by Dojang override any existing values.  This allows hook commands
    and programs invoked by hook scripts to be found through `PATH` as
    documented.  [[#25]]

 -  Fixed `writeManifest` silently dropping file route predicates that could
    not be represented by a moniker or that collided after moniker resolution.
    It now returns a `WriteError`, and `writeManifestFile` raises an I/O error,
    instead of writing an incomplete manifest.  When multiple monikers
    represent the same predicate, the writer now selects the first name in a
    deterministic order.  [[#26]]

[#24]: https://github.com/dahlia/dojang/issues/24
[#25]: https://github.com/dahlia/dojang/issues/25
[#26]: https://github.com/dahlia/dojang/issues/26


Version 0.2.0
-------------

Released on January 19, 2026.

 -  The `dojang init` command now generates *.gitignore* if the source directory
    is a Git repository and the file does not exist yet.  [[#16]]

 -  Fixed a bug that Dojang had not recognized `kernel.release` field in
    a *dojang-env.toml* file unlike how it was documented and how `dojang env`
    command have behaved.  [[#17]]

 -  Added disambiguation for ambiguous source paths in the `dojang reflect`
    command.  When multiple routes map to the same destination, Dojang now:

     -  Auto-selects the route if only one source file exists
     -  Shows an interactive prompt with full source paths and `(exists)` labels
     -  Warns when multiple source files exist (potential configuration issue)
     -  Supports `DOJANG_AUTO_SELECT` environment variable (`first`, `error`)
     -  Accepts `--source` option to explicitly specify which source to use

    [[#22]]

 -  Added the `dojang edit` command which opens the source file of a target
    file in the user's editor and applies changes after editing.  [[#21]]

     -  Editor detection priority: `--editor` option → `$VISUAL` → `$EDITOR`
        → platform default (`notepad` on Windows, `vi` on POSIX)
     -  Use `--no-apply` flag to skip automatic apply after editing
     -  Use `--force` flag to skip conflict warnings
     -  Use `--sequential` flag to edit files one at a time
     -  Use `--source` option or `DOJANG_AUTO_SELECT` to resolve
        ambiguous routes
     -  Non-existent target paths create empty source files before editing
        (prompts for route selection if multiple routes match)

 -  Added a registry file (*~/.dojang*) that stores the repository path.
    This allows the `dojang edit` command to work from any directory.  [[#21]]

     -  Created automatically during `dojang apply`
     -  If the registry points to a different repository, Dojang prompts
        to overwrite (in interactive mode) or overwrites silently
        (in non-interactive mode)

 -  Enhanced `dojang reflect` and `dojang edit` commands to support
    batch operations.  [[#18]]

     -  Without arguments, shows all changed files and prompts for
        confirmation before processing
     -  Use `--all`/`-a` flag to process all changed files without
        prompting
     -  Directory arguments process all changed files within the directory
     -  Ignored files are skipped by default with a warning; use `--force`
        to include them
     -  Use `--include-unregistered`/`-u` flag to include unregistered files
        found in destination directories (prompts for route selection in
        interactive mode; `dojang reflect` only, not yet for `dojang edit`)

 -  Added pre/post apply hooks that run custom scripts before and after
    the `dojang apply` command.  [[#19]]

     -  `pre-apply`: Runs before every apply
     -  `pre-first-apply`: Runs only on first apply (when *~/.dojang*
        doesn't exist)
     -  `post-first-apply`: Runs only on first apply, after file sync
     -  `post-apply`: Runs after every apply
     -  Hooks support conditional execution via `when` field using
        environment predicates
     -  Use `ignore-failure = true` to continue even if a hook fails
     -  In dry-run mode, hooks print “Would run hook: …” instead of
        executing
     -  Environment variables available to hooks: `DOJANG_REPOSITORY`,
        `DOJANG_MANIFEST`, `DOJANG_DRY_RUN`, `DOJANG_OS`, `DOJANG_ARCH`

[#16]: https://github.com/dahlia/dojang/issues/16
[#17]: https://github.com/dahlia/dojang/issues/17
[#18]: https://github.com/dahlia/dojang/issues/18
[#19]: https://github.com/dahlia/dojang/issues/19
[#21]: https://github.com/dahlia/dojang/issues/21
[#22]: https://github.com/dahlia/dojang/issues/22


Version 0.1.0
-------------

Initial release.  Released on November 26, 2023.
