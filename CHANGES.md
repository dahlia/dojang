Dojang changelog
================

Version 0.3.0
-------------

To be released.

 -  Added versioned, repository-scoped machine state under each platform's user
    data directory.  New repositories receive a stable `repository-id`, and
    their full intermediate snapshots now default outside the worktree.  Run
    `dojang migrate` for an existing repository; it copies and verifies the old
    *.dojang/* snapshot before removing it, rejects conflicting copies, and can
    retry an interrupted migration.  It rejects symlinked legacy snapshots and
    overlapping destinations, retains its recovery marker through cleanup, and
    refuses unowned, missing, or invalid snapshots.  Nested symbolic links and
    non-regular entries such as named pipes are rejected even when two existing
    snapshots otherwise match.  Migration markers must also remain regular
    files.  Snapshot aliases are resolved before overlap and cleanup checks,
    and temporary checkout or snapshot aliases are not persisted over stable
    recorded paths.  Symbolic links in private snapshot path components are
    rejected.  Concurrent manifest identity assignment, machine identity
    creation, state writes, and lifecycle writes are serialized; lifecycle
    updates reload the current record instead of restoring stale paths.  An
    empty default snapshot left by an interrupted initial state write can be
    retried safely.  Recovery markers bind retries to their original
    destination, preventing a changed override from deleting an unrelated
    directory.  Migration never follows another registered checkout when
    `-r`/`--repository-dir` is omitted.  Automatic repository selection
    restores an in-checkout recorded manifest path unless
    `-m`/`--manifest-file` is supplied explicitly.  Recorded external manifests
    remain valid evidence when rejecting duplicate live checkouts.  State,
    manifest, and migration-marker replacements use unique sibling temporary
    files; state and manifest replacement also preserves complete POSIX
    permission modes.  Manifests with invalid UTF-8 are rejected without
    replacement.  Native non-UTF-8 path data is preserved, while invalid UTF-8
    and relative checkout, manifest, or intermediate paths in a state record
    are rejected as corruption.  Newer schema versions are identified before
    their version-specific fields are decoded, so they produce upgrade
    guidance rather than repair guidance.  A `state.toml` entry or
    `machine.toml` document that is not a regular file is also rejected instead
    of being followed or treated as absent.  A missing machine identity is
    reported as corruption when repository data remains.  Native user-data
    directory aliases are resolved before state is created, so a valid
    symbolic-link data root works without permitting links inside the private
    state layout. State preflight, root creation and locking, legacy registry
    reads, repository preparation, and first-apply state writes use the
    machine-state diagnostic and exit code 13 instead of escaping as unexpected
    exceptions. State lock paths must be regular files; symbolic links,
    directories, and special files are rejected before opening a lock.
    Manifest read and replacement failures during migration use the manifest
    diagnostic and exit code 11.  Read-only manifests can be replaced when
    their parent directory is writable, including on Windows. The old snapshot
    remains until an intermediate-path override is committed, and override
    copies are journaled even when the destination already matches, so copy or
    cleanup retries do not delete divergent data or strand the old snapshot.
    Repository discovery rejects an incomplete repository entry that has
    recovery data but no `state.toml` record, as well as a nonempty entry whose
    directory name is not a repository UUID, instead of selecting another
    repository.  A `repositories` root that is a file or symbolic link is
    likewise rejected as corruption before a state-reading command can modify
    its checkout, including with explicit repository selection.  Repository-ID
    directories must also be real directories rather than symbolic links.
    Migration resolves path aliases and rejects a machine-state root that
    overlaps the legacy *.dojang/* snapshot before creating state or changing
    the manifest.  It validates an explicitly adopted snapshot before
    publishing state and refuses snapshot locations that could remove the
    checkout or machine-state metadata during later cleanup.  Commands validate
    the stored custom snapshot rather than an unused default path.  A retry can
    finish cleanup when the remaining old snapshot is a matching subset of the
    verified new snapshot. `--dry-run` does not persist migration work.  An
    explicit `--intermediate-dir` is still supported and is now saved for later
    commands.  Existing custom intermediate snapshots can be migrated in place
    by selecting their path explicitly after their complete contents are
    validated.  A legacy *.dojang* path that exists as a non-directory entry is
    rejected instead of being mistaken for an absent snapshot.

    The old *~/.dojang* single-repository pointer no longer selects or records a
    global current repository.  Dojang selects a repository automatically only
    when exactly one valid machine-state record exists and its checkout manifest
    still declares the recorded identity.  A missing or reused checkout path
    requires explicit selection instead of falling back to another directory.
    Otherwise repository commands require `-r`/`--repository-dir`, including
    when one of multiple checkout paths is unavailable.  Checkout aliases
    resolve to the same repository instead of being rejected as duplicates, and
    unrelated data at an old checkout path does not block a legitimate move.
    The manifest path used by each checkout is persisted, so a different
    `-m`/`--manifest` path cannot hide a live duplicate checkout.  Absolute
    manifests outside the recorded checkout cannot validate automatic
    selection.  `dojang init` and `dojang migrate` stay anchored to the current
    or explicitly selected directory, while `dojang env --ignore-env-file`
    bypasses repository selection.  Both orphaned machine state and ambiguous
    repository selection fail with the machine-state exit code before a command
    runs.  Meta commands remain independent of repository state, and explicit
    repository selections do not scan other records.  First-apply lifecycle
    state is now tracked per repository and is preserved from a matching legacy
    registry, including when the registry and command use different aliases of
    the checkout.  A malformed legacy registry must be repaired or removed
    before new machine state can be created.  The registry must be a regular,
    non-symbolic-link file; directories and dangling symbolic links are
    reported as corruption.  `dojang init` performs that check before writing
    its manifest or route directories.  It also refuses a checkout that already
    contains a legacy *.dojang/* snapshot and leaves the snapshot for an
    explicit `dojang migrate`.  [[#34], [#37], [#64]]

 -  Added detailed file and directory routes with ordered `moniker` or inline
    `when` conditions.  Existing compact routes remain valid, while detailed
    routes can represent repeated conditions, direct environment predicates,
    and null routes without losing information.  Manifest writing is now total
    and uses the detailed form whenever the compact form would change a route's
    condition semantics, priority, or multiplicity.  [[#32], [#63]]

 -  Unified `dojang apply` and `dojang reflect` around the shared
    reconciliation planner and executor.  Both commands now inspect conflicts
    and destructive operations before running one ordered plan.  A source-only
    change no longer blocks `dojang reflect`; only incompatible changes on both
    sides count as a conflict.  [[#28], [#31], [#62]]

 -  Added a pure, direction-aware reconciliation planner and a shared
    filesystem operation executor.  Plans expose conflicts, skipped entries,
    and destructive operations before execution, providing the foundation for
    unifying `dojang apply` and `dojang reflect`.  [[#28], [#29], [#61]]

 -  Unified file-state observation and delta calculation for single-file and
    directory routes.  Dojang now detects symbolic links without following
    them, reports removal of every tracked entry kind consistently, and
    compares equal-size regular files by content.  [[#28], [#30], [#60]]

[#28]: https://github.com/dahlia/dojang/issues/28
[#29]: https://github.com/dahlia/dojang/issues/29
[#30]: https://github.com/dahlia/dojang/issues/30
[#31]: https://github.com/dahlia/dojang/issues/31
[#32]: https://github.com/dahlia/dojang/issues/32
[#34]: https://github.com/dahlia/dojang/issues/34
[#37]: https://github.com/dahlia/dojang/issues/37
[#60]: https://github.com/dahlia/dojang/pull/60
[#61]: https://github.com/dahlia/dojang/pull/61
[#62]: https://github.com/dahlia/dojang/pull/62
[#63]: https://github.com/dahlia/dojang/pull/63
[#64]: https://github.com/dahlia/dojang/pull/64


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
