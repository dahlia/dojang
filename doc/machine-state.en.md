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
Running `dojang init` again in an existing repository enrolls the current
machine's [declared facts] instead of rejecting the existing manifest.  The
facts and any associated facts-file path remain attached to the repository
identity when the checkout moves.  `dojang forget` removes that association.

[declared facts]: machine-facts.en.md


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
            targets/
                <route-generation>/
~~~~

*machine.toml* identifies the machine that owns the snapshots.  Each
*state.toml* record has a schema version, repository and machine identities, an
opaque generation UUID, the last known checkout path, the manifest path used by
that checkout, the intermediate snapshot and managed-target baseline roots,
timestamps, an optional facts-file association, declared machine facts, and
typed sections for managed targets, hook execution history, and lifecycle
records.  A new generation UUID is issued whenever a forgotten
repository record is recreated, even when its timestamps match an earlier
record.  Schema version 4 stores machine facts and their file association;
schema version 3 introduced successful `once` and `on-change` hook executions
by event and stable hook identity.  Schema versions 1 through 3 are upgraded in
memory, and their opaque hook data is preserved.  A successful
first apply is recorded per repository, not in a global current-repository
file.

These files are implementation details.  If a record is malformed, uses a
newer schema version, names another repository, or belongs to another machine,
Dojang stops instead of treating it as missing state.  State documents must be
regular files rather than directories or symbolic links, and they must contain
valid UTF-8.  Undecodable bytes are reported as corruption rather than replaced
with another character.  If *machine.toml* is missing while repository data
remains, Dojang reports corruption instead of assigning a new machine identity.
Similarly, a repository directory that contains a migration marker or snapshot
data but no *state.toml* record is reported as an interrupted or corrupted
entry.  Persistent `hook-<sha256>.lock` files are ignored after `dojang forget`
because they contain no repository state.  The only retryable snapshot
exception is an empty private *snapshots/current/* directory left before the
first atomic state write completed.  Discovery checks the entry while holding
its state lock and does not silently select another repository.  A nonempty
entry whose directory name is not a valid repository ID is also reported as
corruption.  The *repositories/* store
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
before they are opened.  Every directory leading to a persisted snapshot must
also be a real directory rather than a symbolic link, including directories
outside the private state store.  The snapshot entry itself cannot be a link.
An explicit custom snapshot alias is resolved before its canonical path is
persisted.  Cleanup checks every component again, so retargeting an ancestor
link cannot redirect recursive deletion.  When an existing checkout or
snapshot is reached through a filesystem alias, Dojang retains the stable
recorded paths instead of persisting the temporary alias.  Stored paths use an
explicit encoding to preserve native path data that is not valid UTF-8.
Checkout, manifest, and intermediate paths must be absolute; a relative value
makes the record malformed.  Managed-target source paths must be relative and
must not contain parent traversal.  Each managed-target baseline path must be a
proper descendant of its recorded baseline root and must not contain parent
traversal.  Dojang reads the schema version before decoding version-specific
fields.  A newer record therefore produces upgrade guidance even when its
remaining layout differs from this release's layout.

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


Managed destinations and orphans
--------------------------------

After each successful `dojang apply` or `dojang reflect`, Dojang records every
entry that actually converged.  A record contains the producing route, whether
that route manages a file or directory, the source entry, the absolute expanded
destination, an immutable baseline, its kind and SHA-256 fingerprint, the
command and time that updated it, and the environment facts used for routing.
A record also retains the route's declared kind and portable mode, so the
metadata a later three-way comparison needs is preserved explicitly instead
of depending on whichever permission bits the host filesystem happens to
preserve.  For a
deployment link, the record stores the link's target string itself as the
fingerprint; that string is the baseline, and no filesystem snapshot is
materialized for it.
Target identifiers preserve platform-native path identity even when a path is
not valid Unicode.  Destination identity follows native filesystem
equality.  Paths that differ only in casing share one identity on Windows, but
remain distinct on POSIX systems.  The same rule is used when deciding whether
a current destination has changed.  Values of referenced environment variables
are never stored verbatim: the record distinguishes unset and empty values and
otherwise stores a SHA-256 digest of the native value.  Distinct native values
remain distinct even when they cannot be decoded as Unicode.  If a multi-entry
operation stops partway through, records for entries that did not converge
retain their preceding values.  Only completed entries receive new records.  A
converged symbolic link is not treated as a deletion because symbolic-link
synchronization is unsupported.  Only convergence on a missing destination
removes its preceding record.  A forced reflection that admits a previously
untracked ignored destination also records that entry.  An argument-free
reflection records every converged routed entry, including unchanged entries
when another entry needed synchronization.  A dry run changes neither these
records nor first-apply history.

Each state update builds its baselines under a new private transaction root.
The records are published only after all required copies finish.  A failed copy
therefore leaves the preceding records and baselines intact.  If publishing the
state record fails after the copies finish, Dojang removes the unpublished
transaction root.  Entries from the same route generation can share a baseline
tree, but destination and route-definition changes use another tree.  A later
synchronization cannot rewrite the evidence used to classify an older orphan.
Before publishing changed records, Dojang journals every baseline and
intermediate path that will become unreachable.  It accepts only paths inside
the recorded snapshot roots.  If post-publication cleanup fails, the journal
remains in *state.toml* and the next stateful command retries it under the same
repository lock.  Once cleanup succeeds, Dojang removes the journal and any
empty generation and transaction directories.

A directory record describes the directory entry itself.  Its descendant
records track the managed files and directories below it.  Status checks do not
recursively compare a directory record's whole subtree, which may include
ignored entries or children synchronized in a later operation.

A record becomes an **orphan** when its route is removed or renamed, becomes
inapplicable or null in the current environment, changes its selected
definition or file/directory type, expands to another destination, or remains
applicable but no longer produces the recorded directory entry.  The last case
can occur after a source entry is removed and its destination becomes ignored.
Ordinary `apply`, `reflect`, and `status` commands never delete an orphan
destination.
Current relative destinations are resolved to absolute paths before this
comparison.  `dojang status` lists each orphan's former destination, source and
route, fingerprint, reason, and whether the destination is unchanged, modified,
or missing.  A named pipe, socket, device, or other special entry at either the
destination or its baseline is classified as modified without reading it.

To stop tracking orphans while keeping their destination files, select a route,
one or more destinations, or both:

~~~~ console
$ dojang unmanage --route config
$ dojang unmanage ~/.config/app/config.toml
~~~~

Selections are combined.  Every selected record must be an orphan; change the
manifest first if a target is still active.  Unknown selections fail without
changing state.  On Windows, destination selectors use native
case-insensitive path comparison.  An unchanged or missing orphan can be
unmanaged directly.  A modified orphan requires `--force`, because discarding
its baseline also discards Dojang's local evidence of the divergence.
`unmanage` removes only selected records, their unreferenced baselines, and
unreferenced entries in the common intermediate snapshot.  A retained directory
record protects its descendants.  Record publication and snapshot cleanup
remain under the same repository lock, so concurrent synchronization cannot
lose a record or make cleanup remove newly live data.  `unmanage` reloads the
selected records and requires every selected ID to remain present after
acquiring that lock.  It then checks their orphan and modification status
again.  Before changing state, it lists every selected record and its current
status.  It never removes source or destination files.

To remove all machine-local state for one repository, use:

~~~~ console
$ dojang forget
~~~~

This removes that repository's target records, intermediate snapshot, managed
target baselines, and first-apply history.  It preserves the machine identity,
every other repository's state, the repository source, and all destinations.
Modified destinations require `--force`.  Validation, snapshot removal, and
state deletion run under one repository lock.  Before deleting the recorded
intermediate snapshot, Dojang rejects symbolic links in the snapshot or any of
its ancestors, along with paths that contain the checkout or machine-state
store.  The private baseline root is removed before the intermediate snapshot.
After validation succeeds, Dojang records the approved cleanup before deleting
either root.  A retry can therefore finish without `--force` when a partial
cleanup has already removed a baseline.  A fresh request still checks every
destination.  While approved cleanup is pending, other stateful commands stop
and ask the user to retry `dojang forget`;
they cannot publish newer records that the earlier approval would discard.
Forgetting an already absent record succeeds without creating machine state or
migrating a legacy snapshot.  If cleanup deleted *state.toml* but left its
approval marker, retrying `dojang forget` clears that marker under the
repository lock without recreating state.
A distinct live checkout with the same `repository-id` cannot forget the
recorded checkout's state.  Successful cleanup removes the empty private
snapshot parent, so automatic repository discovery treats the repository as
forgotten.  Interrupted migration and forget markers are removed under the
repository lock.  If the old *~/.dojang* registry names this checkout, `forget`
also removes that legacy first-apply history.  The next successful apply is
treated as the first apply for this repository.


Choosing another intermediate directory
---------------------------------------

`-i`/`--intermediate-dir` remains available.  The supplied path is a persisted
repository setting, not a one-run override.  A relative path is resolved from
the repository checkout.  Later commands use the stored path until another
explicit path replaces it.  The path cannot be a descendant of *.dojang*
while that directory is the migration source.  A snapshot also cannot contain
the checkout or the machine-state root, and a path inside the state root must
stay within this repository's dedicated *snapshots/* directory.  The
intermediate snapshot and the private managed-target baseline root cannot be
equal or contain one another.  These rules prevent synchronization or later
cleanup from treating baseline data as intermediate files, or from removing
repository files or machine-state metadata.

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
