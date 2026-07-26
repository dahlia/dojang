Bootstrapping a repository
==========================

`dojang init --from SOURCE` brings an existing repository onto a new machine,
enrolls that machine, and offers to run the first `dojang apply`.  Acquisition
happens in an owner-only temporary directory beside the destination.  POSIX
systems request the private mode during creation and then reapply it, so even a
restrictive process umask cannot make the staging directory unusable.  Windows
uses a protected, inheritable owner-only ACL.  A filesystem that cannot enforce
this protection is rejected.  Dojang checks the staged *dojang.toml* before
publishing the repository, so an invalid or incomplete source does not publish
anything at the destination.

The destination must not exist.  Because the default repository directory is
the current directory, use `-r`/`--repository-dir` to select a new path when
bootstrapping.  An existing path is rejected even when it is an empty
directory.  Dojang restores stored permissions while the repository is still
private in staging, then publishes it with one atomic no-replace directory
rename.  If another process creates the destination first, publication fails
without modifying that entry.  Filesystems without an atomic no-replace rename
are rejected instead of using an entry-by-entry fallback.
When cleanup is needed, Dojang first moves the staging tree to an
operating-system-random quarantine name and verifies the moved entry's identity.
It restores a raced replacement instead of deleting it.  If restoration fails,
Dojang leaves the quarantine intact and prints its recovery path.


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
holds each directory open while enumerating it and opens child directories
without following links.  Replacing a directory with a link during traversal
therefore stops or observes the link itself instead of scanning its target.
An entry name that vanishes after enumeration is treated as a concurrent source
change instead of being omitted from the acquired snapshot.
It rejects source paths that collide after case folding or Unicode
normalization before copying any entry.  This prevents distinct names on a
case-sensitive source filesystem from collapsing in staging.  Dojang pins the
identity of the source root and every entry during validation.  Stored
directory permissions come from the same filesystem observation as the
corresponding identity.  Regular files are checked through the same open handle
used to copy them, and the source tree is enumerated again after copying.  If an
entry was added or removed, changed type, or no longer matches its recorded
identity and change metadata, bootstrap stops without publishing the staged
copy.  Dojang also accepts `.zip`, `.tar`, `.tar.gz`, and `.tgz` archives:

~~~~ console
$ dojang -r ~/.dotfiles init --from ~/Downloads/dotfiles.tar.gz
~~~~

An archive source must resolve to a regular file.  Symbolic links to regular
archives are accepted, but FIFOs and other special files are rejected without
reading them.  Dojang checks the opened archive's change metadata after its
bounded read.  If the archive changed during that read, bootstrap stops and
asks you to retry with a stable source.

Archive entries are validated before extraction.  Absolute paths, parent
traversal, backslash paths, links, Windows-reserved or invalid filename
components, and entries that collide case-insensitively or after Unicode
normalization are rejected.  This collision check includes parent directories
that an archive leaves implicit.  Unix ZIP entries that declare FIFOs, sockets,
devices, or another unsupported type are also rejected rather than converted
into regular files.  ZIP directory entries marked with the DOS directory
attribute are accepted even when their names omit the conventional trailing
slash.  Archive input is limited to 16 MiB, expanded file contents to 64 MiB,
the entry count to 10,000, each entry path to 4,096 characters, and path depth
to 256 components.  Compressed tar decoding is bounded as well, so malformed,
highly compressed, or pathologically deep input cannot consume resources
without limit.  On POSIX systems, stored permission bits, including executable
bits, are restored when the destination filesystem supports them.
Bootstrap verifies the resulting permissions instead of assuming that a
successful filesystem call restored every bit.  If exact restoration is
unavailable, bootstrap publishes the contents and warns that the permissions
could not be restored.  Before atomic publication, Dojang reads and parses the
manifest again after applying the final stored permissions.  An archive whose
final file or ancestor permissions make the manifest inaccessible is rejected
while the repository is still in private staging.  A newly created destination
inherits the directory source or tar root permissions.  When the directory
source itself is a symbolic link, these permissions come from the target
directory rather than the link.


External transports
-------------------

Git, HTTPS clients, and other network tools are configured as machine-local
external transports.  Dojang starts the configured executable directly,
without a shell.  The source and staging destination each occupy one complete
argument, so spaces and shell metacharacters remain data.  The destination
given to the transport is a child of an owner-only directory, so files created
with the transport's default permissions are not exposed while acquisition is
in progress.  On POSIX, Dojang passes the native byte representation of the
source, staging destination, and inherited environment values to the child
process without transcoding it through Unicode.

Dojang resolves the destination parent once and records the filesystem identity
of that physical directory and each ancestor before staging begins.  It
rechecks the lexical target and complete physical chain after preparation,
acquisition, validation, and publication, and aborts when it observes a
replacement.  These boundary checks are not an atomic lock: a replacement
between a check and the following filesystem mutation can leave staging or a
published copy at another location.  Dojang warns when cleanup or the
post-publication check detects that situation so the leftover can be removed
manually.

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

The selected manifest path must be relative without a drive prefix, remain
inside the acquired repository, and contain no parent or symbolic-link
components.  This ensures staged validation and post-publication enrollment
observe the same manifest.  The manifest must be a regular file no larger than
16 MiB.  Dojang parses the bytes read through that validated handle, rejecting
a FIFO or device without reading it and rejecting in-place changes observed
during the read.

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
It applies the same requirement that the destination does not exist, then
models acquisition and publication without changing the filesystem.
For local directory and archive sources, virtual publication retains the staged
root and nested directory permissions, so enrollment and apply previews observe
the same metadata as a real rename.
For an external transport, Dojang redacts the source argument and environment
values when it prints the executable request, including source arguments whose
native representation is not UTF-8, and does not start the program.  Because
no files are fetched, that dry run cannot validate the remote manifest.  Run a
real bootstrap to perform the staged manifest check.
