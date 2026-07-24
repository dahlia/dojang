Bootstrapping a repository
==========================

`dojang init --from SOURCE` brings an existing repository onto a new machine,
enrolls that machine, and offers to run the first `dojang apply`.  Acquisition
happens in an owner-only temporary directory beside the destination.  Dojang
checks the staged *dojang.toml* before publishing the repository, so an invalid
or incomplete source does not replace the destination.

The destination must not exist or must be an empty directory.  It is the current
directory unless `-r`/`--repository-dir` selects another path.  Publication
refuses a destination that has been replaced by a symbolic link.  Other
existing empty destinations are replaced atomically; if another process creates
the destination during publication, Dojang fails instead of replacing it.  The
current directory keeps its identity.  Publication requires the destination
filesystem to support an atomic no-replace rename when replacing the
destination as a whole.  Dojang fails closed when that operation is unavailable
instead of falling back to a race-prone rename.  When publishing into the
current directory, each entry is created without replacement.  If another
process creates an entry first, bootstrap fails without overwriting it; cleanup
removes only unchanged entries created by bootstrap.  Entries replaced by
another process are preserved, and interruption triggers the same rollback.


Local directories and archives
------------------------------

Local directories need no transport option:

~~~~ console
$ dojang -r ~/.dotfiles init --from /media/backup/dotfiles
~~~~

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
into regular files.  On POSIX systems, stored permission bits, including
executable bits, are restored when the destination filesystem supports them.
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
argument, so spaces and shell metacharacters remain data.

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
For an external transport, Dojang redacts the source argument and environment
values when it prints the executable request, and does not start the program.
Because no files are fetched, that dry run cannot validate the remote manifest.
Run a real bootstrap to perform the staged manifest check.
