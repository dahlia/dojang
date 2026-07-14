Machine state
=============

Dojang keeps each repository's intermediate snapshot and local metadata in the
platform's user data directory.  This state does not travel with the repository.
The `repository-id` in *dojang.toml* does: it is a stable UUID that lets Dojang
find the right local state after a checkout is moved or renamed.

When exactly one repository is registered, repository commands can select its
recorded checkout automatically.  Automatic selection also restores the
recorded manifest path when it is inside the checkout, including a custom
manifest name.  A recorded external manifest still proves ownership when
detecting duplicate live checkouts, but automatic selection requires explicit
`-r`/`--repository-dir` and `-m`/`--manifest-file` options for that layout.

`dojang init` creates the identity and state record for a new repository.  Do
not copy an identity into an unrelated repository.  Two existing checkouts with
the same identity on one machine are treated as an error.  Paths that resolve
to the same checkout, including symbolic-link aliases, are not duplicates.


Storage layout
--------------

The state root follows the native convention for each platform:

 -  Linux: `$XDG_DATA_HOME/dojang`, or *~/.local/share/dojang* when
    `XDG_DATA_HOME` is unset, empty, or relative
 -  macOS: *~/Library/Application Support/dojang*
 -  Windows: `%LOCALAPPDATA%\dojang`, or *~/AppData/Local/dojang* when
    `LOCALAPPDATA` is unset, empty, or relative

Dojang resolves filesystem aliases in the state root before creating this
layout.  The native user data directory may therefore be a symbolic link,
while symbolic links inside Dojang's private state layout remain invalid.

The private layout under that root is:

~~~~ text
machine.toml
machine.lock
manifest-identity.lock
repositories/
    <repository-id>/
        state.toml
        state.lock
        snapshots/
            current/
~~~~

*machine.toml* identifies the machine that owns the snapshots.  Each
*state.toml* record has a schema version, repository and machine identities,
the last known checkout path, the manifest path used by that checkout, the
intermediate snapshot path, timestamps, and typed sections reserved for target,
hook, and lifecycle records.  A successful first apply is recorded per
repository, not in a global current-repository file.

These files are implementation details.  If a record is malformed, uses a
newer schema version, names another repository, or belongs to another machine,
Dojang stops instead of treating it as missing state.  State documents must be
regular files rather than directories or symbolic links, and they must contain
valid UTF-8.  Undecodable bytes are reported as corruption rather than replaced
with another character.  If *machine.toml* is missing while repository data
remains, Dojang reports corruption instead of assigning a new machine identity.
Similarly, a repository directory that contains a migration marker or snapshot
data but no *state.toml* record is reported as an interrupted or corrupted
entry.  The only retryable exception is an empty private *snapshots/current/*
directory left before the first atomic state write completed.  Discovery
checks the entry while holding its state lock and does not silently select
another repository.  A nonempty entry whose directory name is not a valid
repository ID is also reported as corruption.  The *repositories/* store
itself must be a regular directory; a file or symbolic link at that path is not
treated as an empty store.  Commands that use machine state validate the store
before modifying a checkout, including when `-r`/`--repository-dir` selects the
repository explicitly.  A directory named with a valid repository ID must also
be a real directory rather than a symbolic link.
If a state path cannot be inspected, created, read, or updated because of an
I/O failure, Dojang reports a machine-state error with exit code 13 instead of
an unexpected-error exception.
Repository records are replaced atomically, so an interrupted write leaves the
previous complete record in place.  Locks serialize concurrent identity and
state updates, and each update uses its own temporary file.  Lock entries must
be regular files; symbolic links, directories, and special files are rejected
before they are opened.  Every component of a private snapshot path must also
be a real directory rather than a symbolic link.  When an existing checkout or
snapshot is reached through a filesystem alias, Dojang retains the stable
recorded paths instead of persisting the temporary alias.  Stored paths use an
explicit encoding to preserve native path data that is not valid UTF-8.
Checkout, manifest, and intermediate paths must be absolute; a relative value
makes the record malformed.  Dojang reads the schema version before decoding
version-specific fields.  A newer record therefore produces upgrade guidance
even when its remaining layout differs from this release's layout.

Dojang stops if a recorded intermediate snapshot is missing or has been
replaced by a regular file.  It does not treat a missing common ancestor as an
empty snapshot.


Migrating an existing repository
--------------------------------

Run migration explicitly from the existing checkout:

~~~~ console
$ dojang -r /path/to/dotfiles migrate
Stable repository identity added to /path/to/dotfiles/dojang.toml.
Repository machine state is ready at .../snapshots/current.
~~~~

Use `--dry-run` first to inspect the file operations without changing the
manifest, snapshot, or state store:

~~~~ console
$ dojang --dry-run -r /path/to/dotfiles migrate
Stable repository identity added to /path/to/dotfiles/dojang.toml.
Repository machine state is ready at .../snapshots/current.
Note: Since --dry-run was specified, those 8 changes were not actually
committed to the filesystem.
~~~~

The machine-state root must not overlap the legacy *.dojang/* snapshot.  Dojang
resolves path aliases and checks this before creating the state directory or
adding an identity to the manifest.  If `XDG_DATA_HOME` or `LOCALAPPDATA` puts
the state root inside *.dojang/*, choose a native data directory outside the
snapshot before migrating.

Migration adds `repository-id` without rewriting the rest of *dojang.toml*.
The updated manifest is written to a sibling temporary file and replaces the
original atomically while retaining its file permissions, including the full
POSIX mode.  The manifest must be valid UTF-8.  Migration rejects undecodable
bytes without replacing the original file.  A failure to read or atomically
replace the manifest is reported as a manifest error with exit code 11, and the
original remains in place.  If the checkout contains a legacy *.dojang/*
snapshot, Dojang copies it to the external store, compares the complete copies,
writes the state record, and only then removes the old directory.  Read-only
manifest permissions are applied after the new contents are written, so a
writable parent directory is sufficient for the atomic replacement.  An
interruption leaves a migration marker and at least one complete copy; run the
same command again to retry.

The marker is published with an atomic sibling-file replacement, so a failed or
interrupted write does not expose a partial marker.  It records both the source
and destination and remains until cleanup finishes.  Retry with the same
`-i`/`--intermediate-dir` value, if one was used.
Dojang requires the marker to remain a regular file.  It reports a directory,
symbolic link, named pipe, or other non-regular entry as corrupted state before
reading it.
Dojang rejects a different retry destination before removing anything.  If the
destination contains only a matching subset of the source, Dojang recognizes
an interrupted partial copy and starts it again.  If an existing destination
contains a different entry, Dojang keeps the marker and both copies, then asks
you to resolve the conflict.  An external snapshot without either a state
record or a legacy source is also rejected instead of being adopted as the
common ancestor.  If cleanup was interrupted after removing part of the old
snapshot, a retry removes the remainder only when every remaining entry matches
the complete destination.

If an older Dojang release used a custom intermediate directory, pass that
same directory while migrating:

~~~~ console
$ dojang -r /path/to/dotfiles -i /path/to/existing-snapshot migrate
Stable repository identity added to /path/to/dotfiles/dojang.toml.
Repository machine state is ready at /path/to/existing-snapshot.
~~~~

Dojang validates an explicitly selected existing snapshot, then records it in
place.  It does not require a *.dojang/* directory or copy the custom snapshot.
Snapshots can contain regular files and directories only.  Symbolic links,
named pipes, sockets, and device nodes are rejected before Dojang reads or
copies them.  Without `-i`, an existing external snapshot that has no state
record remains unowned and is rejected.

Dojang does not migrate symbolic links in a snapshot, including links nested
inside otherwise matching copies.  A *.dojang* directory that is itself a
symbolic link is rejected before its target is read or removed.  A *.dojang*
path that exists as a regular file or another non-directory entry is also
rejected rather than treated as a missing snapshot.

Dojang refuses migration when the old and new locations contain different
snapshots.  It leaves both unchanged.  Preserve both copies, decide which one
is the correct common ancestor, remove or relocate the other copy, and run
`dojang migrate` again.  If cleanup fails after the state record is written,
keep the old directory as a recovery copy until the external snapshot has been
checked.

The old *~/.dojang* registry is recognized only to preserve migration history
and print migration guidance.  It no longer chooses one global current
repository.  A malformed registry must be repaired or removed before Dojang
creates a repository state record; it is ignored once that record exists.  The
registry must be a regular, non-symbolic-link file.  Directories, dangling
symbolic links, and other non-regular entries are reported as corruption before
their contents are read.  When `dojang init` creates a new repository, it
performs this check before writing the manifest or route directories.  It also
refuses a checkout that already contains *.dojang/* and leaves that legacy
snapshot untouched for an explicit `dojang migrate`.  When exactly one valid
repository record exists, Dojang uses its last known checkout automatically
after confirming that the manifest at its recorded path declares the stored
`repository-id`.  A later `-m`/`--manifest` value does not replace that
ownership check.  If the checkout is missing or now belongs to another
repository, Dojang stops and asks for an explicit `-r`/`--repository-dir`
selection.  With two or more records, repository commands also require explicit
selection, even when a recorded checkout is temporarily missing.  Meta commands
such as `dojang version` and `dojang help` do not open the machine-state
store.  An explicit repository selection does not scan unrelated repository
records.  An absolute manifest can validate an automatically selected checkout
only when the resolved manifest path is inside that checkout.  Use `-r` with a
manifest stored elsewhere.

`dojang init` and `dojang migrate` validate the machine-state store but do not
automatically select a registered checkout.  Without `-r`, each command uses
the current directory.  `dojang env --ignore-env-file` reads only the host
environment, so it does not open machine state or require repository selection.

When an explicitly selected checkout has moved, Dojang treats the old path as a
duplicate only if the manifest at its recorded path is readable and still
declares the same `repository-id`.  Unrelated data that later occupies the old
path does not block the move.

If the legacy registry points to the checkout being migrated, Dojang preserves
its completed first-apply status.  Checkout aliases are resolved before this
comparison.  The migration does not rerun one-time hooks that the old version
already ran successfully.  Concurrent lifecycle updates reload the current
record before changing first-apply status, so they do not restore an obsolete
snapshot path.

Concurrent migrations share a lock while reading or assigning the manifest
identity.  Only one UUID can be written, and every process reloads that identity
before preparing repository state.


Choosing another intermediate directory
---------------------------------------

`-i`/`--intermediate-dir` remains available.  The supplied path is a persisted
repository setting, not a one-run override.  A relative path is resolved from
the repository checkout.  Later commands use the stored path until another
explicit path replaces it.  The path cannot be a descendant of *.dojang*
while that directory is the migration source.  A snapshot also cannot contain
the checkout or the machine-state root, and a path inside the state root must
stay within this repository's dedicated *snapshots/* directory.  These rules
prevent later cleanup from removing repository files or machine-state
metadata.

Dojang applies the same copy, comparison, and conflict rules when changing a
persisted intermediate path.  The old and new paths cannot contain one another.
Dojang resolves filesystem aliases before checking this rule and never removes
one alias of a snapshot as though it were a separate old copy.  It keeps the old
snapshot until the updated state record points to the verified new copy.  It
validates the actual stored custom path when `-i` is omitted; an unused default
path does not affect that repository.  It writes a migration marker before
copying.  An interrupted or failed copy can be retried with the same
`-i`/`--intermediate-dir` value.  When the destination
already contains a complete matching copy, Dojang still writes the marker
before updating the state record.  A later invocation can therefore finish
removing the old snapshot if cleanup was interrupted.  A retry discards only a
destination whose existing entries all match the source; divergent data is left
in place and reported as a conflict.  This option can place machine state inside
a worktree, but doing so may let a transport such as a file synchronization
service copy machine-specific history to another device.  The external default
avoids that risk.
