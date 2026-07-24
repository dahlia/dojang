Bootstrapping a repository
==========================

`dojang init --from SOURCE` brings an existing repository onto a new machine,
enrolls that machine, and offers to run the first `dojang apply`.  Acquisition
happens in an owner-only temporary directory beside the destination.  POSIX
systems request the private mode during creation and then reapply it, so even a
restrictive process umask cannot make the staging directory unusable.  Windows
uses a protected, inheritable owner-only ACL.  A filesystem that cannot enforce
this protection is rejected.  Dojang checks the staged *dojang.toml* before
publishing the repository, so an invalid or incomplete source does not replace
the destination.

The destination must not exist or must be an empty directory.  It is the current
directory unless `-r`/`--repository-dir` selects another path.  Publication
refuses a destination that has been replaced by a symbolic link.  A missing
destination is published with an atomic no-replace rename.  An existing empty
destination is atomically exchanged with staging when the filesystem supports
directory exchange.  If atomic exchange is unavailable, bootstrap rejects that
existing destination instead of using a race-prone fallback.  If another
process adds an entry to the old destination during that exchange, bootstrap
reverses the exchange instead of deleting the entry.  The current directory
keeps its identity by moving each complete top-level staging entry with an
atomic no-replace rename.  If another process creates an entry first, bootstrap
fails without overwriting it.  Stored permissions are restored while the entries
are still private in staging.  Bootstrap captures entry identities before
publication, and rollback atomically moves entries back into private quarantine
before validating those identities and deleting anything.  Entries replaced by
another process are restored instead, and interruption triggers the same
rollback.  If a concurrent replacement cannot be restored safely, Dojang keeps
the private staging tree and reports its location instead of deleting the
recovery data.  Windows does not currently provide directory exchange through
Dojang, so use an absent non-current-directory destination or run bootstrap from
inside an existing empty destination.


Local directories and archives
------------------------------

Local directories need no transport option:

~~~~ console
$ dojang -r ~/.dotfiles init --from /media/backup/dotfiles
~~~~

Dojang rejects a destination nested inside its local directory source before
creating staging.  Choose a sibling or otherwise disjoint destination instead.

Dojang copies symbolic links as links instead of following them.  Other entries
in a directory source must be regular files or directories; FIFOs, sockets,
devices, and other special files are rejected before they are read.  Dojang
also accepts `.zip`, `.tar`, `.tar.gz`, and `.tgz` archives:

~~~~ console
$ dojang -r ~/.dotfiles init --from ~/Downloads/dotfiles.tar.gz
~~~~

An archive source must resolve to a regular file.  Symbolic links to regular
archives are accepted, but FIFOs and other special files are rejected without
reading them.

Archive entries are validated before extraction.  Absolute paths, parent
traversal, backslash paths, links, Windows-reserved or invalid filename
components, and entries that collide case-insensitively or after Unicode
normalization are rejected.  This collision check includes parent directories
that an archive leaves implicit.  Unix ZIP entries that declare FIFOs, sockets,
devices, or another unsupported type are also rejected rather than converted
into regular files.  ZIP directory entries marked with the DOS directory
attribute are accepted even when their names omit the conventional trailing
slash.  Archive input is limited to 16 MiB, expanded file contents to 64 MiB,
and the entry count to 10,000.  Compressed tar decoding is bounded as well, so
malformed or highly compressed input cannot expand without limit.  On POSIX
systems, stored permission bits, including executable bits, are restored when
the destination filesystem supports them.
Bootstrap verifies the resulting permissions instead of assuming that a
successful filesystem call restored every bit.  If exact restoration is
unavailable, bootstrap publishes the contents and warns that the permissions
could not be restored.  A newly created destination inherits the directory
source or tar root permissions.  When the directory source itself is a symbolic
link, these permissions come from the target directory rather than the link.
When replacing an existing empty destination, its root directory keeps its
original permissions.


External transports
-------------------

Git, HTTPS clients, and other network tools are configured as machine-local
external transports.  Dojang starts the configured executable directly,
without a shell.  The source and staging destination each occupy one complete
argument, so spaces and shell metacharacters remain data.  The destination
given to the transport is a child of an owner-only directory, so files created
with the transport's default permissions are not exposed while acquisition is
in progress.

The default configuration file is:

 -  Linux and other POSIX systems:
    *$XDG\_CONFIG\_HOME/dojang/transports.toml*, or
    *~/.config/dojang/transports.toml* when `XDG_CONFIG_HOME` is unset or
    relative
 -  macOS: *~/Library/Application Support/dojang/transports.toml*
 -  Windows: *%APPDATA%\\dojang\\transports.toml*, falling back to
    *%USERPROFILE%\\AppData\\Roaming\\dojang\\transports.toml*

For example, this configuration adds a Git transport:

~~~~ toml
[transports.git]
command = ["git", "clone", "--", "{source}", "{destination}"]
inherit-environment = ["HOME", "PATH", "SSH_AUTH_SOCK"]

[transports.git.environment]
GIT_TERMINAL_PROMPT = "1"
~~~~

Each command must contain exactly one whole-argument `{source}` placeholder and
one whole-argument `{destination}` placeholder.  The executable cannot contain
a placeholder.  A transport receives no host environment variables unless
their names appear in `inherit-environment`; values in `environment` override
inherited values.  Keep credentials out of this file.

Use the transport by name:

~~~~ console
$ dojang -r ~/.dotfiles init \
>   --from git@github.com:USER/dotfiles.git \
>   --transport git
~~~~

Use `--transport-file PATH` with `--transport NAME` to select another
configuration file.  Transport names are case-sensitive.  They begin with an
ASCII letter and may contain ASCII letters, digits, hyphens, and underscores.
The names `directory` and `archive` are reserved.


Enrollment and the first apply
------------------------------

After acquisition, `dojang init` validates the manifest and enrolls the current
machine.  It accepts the same `--fact KEY=VALUE` and `--facts-file PATH`
options as ordinary repository initialization.  Dojang then asks whether to
apply the repository.  Declining leaves a valid, enrolled checkout without
changing target files.

Noninteractive bootstrap requires explicit approval:

~~~~ console
$ dojang -r ~/.dotfiles init \
>   --from /media/backup/dotfiles.tar.gz \
>   --no-interactive \
>   --yes
~~~~

`--yes` accepts only the first mutating apply.  It does not bypass manifest,
fact, destination, transport, or archive validation.


Previewing a bootstrap
----------------------

Put the global `--dry-run` option before `init`:

~~~~ console
$ dojang --dry-run -r ~/.dotfiles init --from /media/backup/dotfiles
~~~~

A dry run does not publish the repository, save enrollment, or apply files.
On Linux and macOS, the in-memory preview models directory exchange, so an
existing empty destination can be previewed without changing it.  The real
bootstrap still verifies that the destination filesystem supports the atomic
operation.
For an external transport, Dojang redacts the source argument and environment
values when it prints the executable request, and does not start the program.
Because no files are fetched, that dry run cannot validate the remote manifest.
Run a real bootstrap to perform the staged manifest check.
