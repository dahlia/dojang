Three-way merge
===============

`dojang merge` resolves files changed in both the repository and their
destinations.  It uses the intermediate snapshot as the common ancestor, runs
a configured merge driver, and writes an accepted result to the source,
destination, and intermediate snapshot.


Using the command
-----------------

Run the command without paths to select every current three-way conflict:

~~~~ console
$ dojang merge
~~~~

Pass a source path, destination path, or containing directory to limit the
selection.  Multiple paths are allowed:

~~~~ console
$ dojang merge HOME/.bashrc ~/.config/example/
~~~~

`--driver NAME` selects a named driver instead of the configured default.
`--driver-file PATH` reads a different configuration file.  `--dry-run`
validates the selected conflicts and driver configuration, then prints the
planned merges without creating a workspace, starting the driver, or changing
files and machine state.

Version 0.3 supports regular UTF-8 text files on `copy` routes that use the
`identity` codec.  The source, destination, and intermediate snapshot must all
exist.  Directory routes, deployment links, transformed codecs, binary files,
invalid UTF-8, and conflicts without a common intermediate snapshot are
rejected before any merge driver starts.  If an unselected unsupported
conflict blocks a command without paths, pass the paths of the supported
conflicts explicitly.


Configuring drivers
-------------------

Merge drivers are machine-local.  The default configuration path is:

| Platform        | Path                                                      |
| --------------- | --------------------------------------------------------- |
| Windows         | `%APPDATA%\dojang\merge-drivers.toml`                     |
| macOS           | `~/Library/Application Support/dojang/merge-drivers.toml` |
| Other platforms | `$XDG_CONFIG_HOME/dojang/merge-drivers.toml`              |

Windows falls back to `%USERPROFILE%\AppData\Roaming` when `APPDATA` is
missing or relative.  Other platforms fall back to `~/.config` when
`XDG_CONFIG_HOME` is missing or relative.

The file names a default driver and defines one or more drivers:

~~~~ toml
default-driver = "example"

[merge-drivers.example]
command = [
  "my-merge-driver",
  "{result}",
  "{base}",
  "{source}",
]
inherit-environment = ["PATH"]
unresolved-exit-codes = [1]
canceled-exit-codes = [130]

[merge-drivers.example.environment]
LC_ALL = "C"
~~~~

`command` is an argument array, not a shell command.  `{source}`, `{base}`, and
`{result}` must each appear exactly once as whole arguments.
`{destination}` is optional and may appear once.  The result file starts as a
copy of the destination.  A driver resolves the conflict by updating that
file and returning zero.

The child process receives only variables named by `inherit-environment` plus
the fixed values in `environment`.  Fixed values override inherited values.
Codes from 1 through 255 in `unresolved-exit-codes` mean the conflict remains
unresolved; codes in `canceled-exit-codes` use the same range and mean the user
canceled the merge.  The two lists must not overlap.


Safety and recovery
-------------------

Dojang validates every selected input before starting the first driver.  Each
driver receives private copies in an owner-only workspace and runs without a
shell.  If an input disappears or cannot be read while it is being captured,
Dojang reports a conflict instead of an internal error.  Before each accepted
result is written, Dojang checks that the authoritative files still match the
bytes, identities, and modes observed during validation.  After the driver
exits, it also reloads the routing context and rejects the result if the
selected route or its resolved paths, kind, mode, codec, or provenance changed.
A changed repository, machine, or state generation identity also rejects the
result.  Final replica writes hold the repository-generation lock, so
`dojang forget` cannot approve deletion while a merge commit is in progress.
Target publication checks the captured generation again under its state-update
lock and rejects data from a forgotten and recreated generation.

Results are committed in source, destination, then intermediate order.  This
order prevents the intermediate snapshot from claiming convergence before
both authoritative copies contain the result.  If a later write fails, earlier
writes remain.  A retry recognizes the case where both authoritative copies
already contain the result but the destination mode or intermediate content
or mode is still stale.  Rerun `dojang merge` or inspect the reported workspace
to recover the operation.  Failed, unresolved, and canceled workspaces are
retained and printed in the error output.  Successful workspaces are removed.
`dojang forget` removes every retained merge workspace for the repository.
Before recursively removing one, it verifies the workspace directory and its
complete ancestor chain, and refuses cleanup if a symbolic link could redirect
deletion outside machine-local state.
Immediately before publishing machine state, Dojang re-observes all three
replicas.  Lost convergence reports a conflict and keeps the recovery journal.
The pending-publication marker also remains after a guarded commit abort and is
removed only after target publication succeeds, so a later converged state can
still repair its machine-state record.
Pending-publication recovery scans only the known invocation and conflict
directory levels; it does not recurse into subdirectories created by a driver.
Workspace setup removes partial private copies when possible.  Later filesystem
failures retain the workspace and use exit status 2.  A failure while removing
a completed invocation workspace uses the same status and identifies the
workspace root to inspect.  A driver result that cannot be read is invalid
output instead: it is rejected before any replica write and uses exit status 4.

The command runs `pre-merge` hooks before loading the context used for conflict
selection and `post-merge` hooks after a successful command.  See [hooks] for
configuration.

[hooks]: hooks.en.md


Exit status
-----------

The most relevant exit codes are:

 -  `1`: invalid driver configuration or selection.
 -  `2`: filesystem write failure.
 -  `4`: the driver failed, could not start, or produced an invalid result.
 -  `13`: machine state could not be updated.
 -  `30`: a conflict is unsupported, unresolved, or changed concurrently.
 -  `32`: a selected path is not routed.
 -  `36`: the driver reported cancellation.
 -  `40`: a merge hook failed.

See [exit codes] for the complete list.

[exit codes]: exit-codes.en.md
