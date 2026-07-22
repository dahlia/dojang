Manifest
========

A **manifest** file, named *dojang.toml*, declares that the directory in which
it resides is a repository for config files managed by Dojang, and sets up how
those config files should be applied on the actual machine.

It is in [TOML] format, as you might guess from the extension, and is divided
into seven main sections: `vars`, `dirs`, `files`, `monikers`, `ignores`,
`codec-backends`, and `hooks`.
This document assumes that the reader knows the basic syntax of TOML.

[TOML]: https://toml.io/


Repository identity
-------------------

The top-level `repository-id` field is a UUID created by `dojang init` or
`dojang migrate`:

~~~~ toml
repository-id = "123e4567-e89b-42d3-a456-426614174000"
~~~~

This identity travels with the repository and remains unchanged when its
checkout is moved or renamed.  Do not edit it or copy it into another
repository.  Dojang uses it to isolate the repository's [machine state] on each
device.

[machine state]: machine-state.en.md


Codec backends
--------------

The optional `codec-backends` table declares commands used by the built-in
encrypted and secret-template codecs.  Each key is a manifest-local backend
name:

~~~~ toml
[codec-backends.vault]
command = "$HOME/.local/bin/dojang-vault"
version = "2026-07"
timeout-seconds = 45
options = { profile = "work", strict = true }
~~~~

`command` is a file path expression and must expand to an absolute executable
path.  `version` is a nonempty identity supplied by the backend operator;
change it whenever backend behavior or key material changes.  The optional
`timeout-seconds` field ranges from 1 to 300 and defaults to 30.  `options`
accepts codec configuration values but must not contain secrets because it is
sent in the protocol header and may affect diagnostic identities.

Dojang invokes a backend without a shell, arguments, or inherited environment.
The working directory is the repository root.  Sensitive codec backends are
not available on Windows in this release.  See the [codec backend protocol] for
the security boundary and recovery procedure.

[codec backend protocol]: codecs.en.md#sensitive-codec-backends


Moniker
-------

A **moniker** is the name given to the criteria for
an [**environment**](environment.en.md).  How the moniker is named is up to you,
but it should be easy to recognize.  For example, below are some of the most
common and simple moniker definitions:

~~~~ toml
[monikers.windows]
os = "windows"

[monikers.posix]
os = ["linux", "macos"]
~~~~

These monikers allow you to conditionally apply config files based on
the environment.  For example, if a user has an older Intel MacBook Pro and
a newer Apple silicon MacBook Pro, they would be able to define the monikers
like below:

~~~~ toml
[monikers.mac]
os = "macos"

[monikers.apple_silicon_mac]
any = ["mac"]
arch = "aarch64"

[monikers.intel_mac]
any = ["mac"]
arch = "x86_64"
~~~~

In the example above, the `any` field is used to refer to another moniker
prerequisite.  It is satisfied if any of the monitors in the list are satisfied.
If you want a predicate that requires all of the listed monitors to be
satisfied, you can use the `all` field.

If you have more complex criteria for the environment, you can use
the [**environment predicate**](environment-predicate.en.md) syntax in the
`when` field.  In fact, the above example can be rewritten with the environment
predicate syntax as shown below and it will work the same:

~~~~ toml
[monikers.mac]
when = "os = macos"

[monikers.apple_silicon_mac]
when = "moniker = mac && arch = aarch64"

[monikers.intel_mac]
when = "moniker = mac && arch = x86_64"
~~~~

For a detailed description of the environment predicate syntax,
see the [corresponding documentation](environment-predicate.en.md).

### Fields that can be used when defining a moniker

 -  `os` (string or array of strings): The operating system of the environment.
    If it is an array, it is satisfied if any of the operating systems in the
    array are satisfied.  The OS identifiers are listed in
    the [List of recognized operating
    systems](environment.en.md#list-of-recognized-operating-systems) section.

 -  `arch` (string or array of strings): The processor architecture of the
    environment.  If it is an array, it is satisfied if any of the architectures
    in the array are satisfied.  The architecture identifiers are listed
    in the [List of recognized processor
    architectures](environment.en.md#list-of-recognized-processor-architectures)
    section.

 -  `kernel` (string or array of strings): The kernel name of the environment.
    If it is an array, it is satisfied if any of the kernels in the array are
    satisfied.
    See also [Kernel recognition](environment.en.md#kernel-recognition) section.

 -  `kernel-release` (string or array of strings): The kernel version of the
    environment.  If it is an array, it is satisfied if any of the kernel
    versions in the array are satisfied.
    See also [Kernel recognition](environment.en.md#kernel-recognition) section.

 -  `any` (array of strings): Other moniker names.  It is satisfied if any of
    the monikers in the array are satisfied. There must be at least one element.

 -  `all` (array of strings): Other moniker names.  It is satisfied if all of
    the monikers in the array are satisfied. There must be at least one element.

 -  `when` (string): Criteria written in the [environment
    predicate](environment-predicate.en.md) syntax.  It is satisfied if
    the criteria are satisfied.  If used with the fields above,
    all criteria together must be satisfied.


Manifest variables
------------------

The optional `vars` section declares reusable values for
[file path expressions].  A compact declaration is an unconditional value:

~~~~ toml
[vars]
CONFIG_HOME = "${XDG_CONFIG_HOME:-$HOME/.config}"
CACHE_HOME = "$CONFIG_HOME/cache"
~~~~

Names are case-sensitive and must match `[A-Za-z_][A-Za-z0-9_]*`.  References
use the same `$NAME` or `${NAME}` syntax as inherited environment variables.
Declarations may refer to one another in any table order.  A reference cycle
is an error, and Dojang reports the cycle instead of applying any files.

Use an ordered array of branch tables when a value depends on the environment:

~~~~ toml
[vars]
TOOLS_HOME = [
  { when = "fact.class = work", value = "$HOME/work/tools" },
  { moniker = "windows", value = "$UserProfile/tools" },
  { when = "always", value = "$HOME/tools" },
]
~~~~

Each branch must contain `value` and exactly one of `moniker` or `when`.
Branches are tried in declaration order, and the first match wins.  If no
branch matches, lookup falls through to an inherited environment variable of
the same name.  A selected empty string is different: it is a defined empty
value and shadows the inherited variable.

Manifest variables take precedence over inherited environment variables.
Machine facts and other environment predicates select branches; they are not
automatically exposed as variable values.  `dojang init` asks for custom facts
referenced by reachable variable branches.  Variable values are configuration,
not secrets: Dojang avoids storing their plaintext in route records and hook
fingerprints, but processes and diagnostics can still observe expanded paths.

[file path expressions]: file-path-expression.en.md


Routing of directories and files
--------------------------------

[**Routing**](routing.en.md) is for applying config files to different paths
depending on the environment.  For example, the home directory is different on
all three platforms Linux, macOS, and Windows, so it can be routed as follows:

~~~~ toml
[dirs.home_directory]
linux = "/home/$USER"
mac = "/Users/$USER"
win = "C:\\Users\\$UserName"

[monikers.linux]
os = "linux"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

Syntax like `$USER` or `$UserName` will be replaced with the value of
the corresponding environment variable, which is explained in more detail
in the document for the [file path expression](file-path-expression.en.md).

Of course, the above example is artificially crafted for illustrative purposes,
and in practice we would do something simpler and more accurate like below:

~~~~ toml
[dirs.home_directory]
posix = "$HOME"
windows = "$UserProfile"

[monikers.posix]
os = ["linux", "macos"]

[monikers.windows]
os = ["windows"]
~~~~

This compact route syntax maps each moniker directly to a path.  It remains the
shortest form when every branch uses a different moniker.  To use an
environment predicate directly, preserve repeated conditions, or mix both
forms in one route, write the route as an ordered array of branch tables:

~~~~ toml
[[files.".bashrc"]]
moniker = "posix"
path = "$HOME/.bashrc"

[[files.".bashrc"]]
when = "os = windows && arch = aarch64"
path = "$UserProfile/.bashrc"

[[files.".bashrc"]]
when = "os = android"
~~~~

Each detailed branch must have exactly one of `moniker` or `when`.  A
`moniker` value must name a moniker defined in the same manifest, while `when`
accepts an [environment predicate](environment-predicate.en.md).  The `path`
field is optional.  Omitting it creates a null route for that condition.
Writing `path = ""` instead specifies an explicit empty path expression.
Dojang preserves detailed branches in their route order, including repeated
conditions.  Branches are tried in that order, and the first matching branch
determines the destination.  Compact routes continue to use predicate
specificity to determine priority.

### Destination modes and kinds

A detailed branch may also declare metadata for its destination through the
optional `mode` and `kind` fields.  Both require a `path`, so a null route
cannot carry metadata.  Compact routes cannot declare metadata; use the
detailed form instead.

The `mode` field declares a portable permission intent for the destination
from a small closed vocabulary:

| `mode`                 | POSIX file | POSIX directory | Windows             |
| ---------------------- | ---------- | --------------- | ------------------- |
| `"default"` (omitted)  | unmanaged  | unmanaged       | unmanaged           |
| `"private"`            | `0600`     | `0700`          | not enforceable     |
| `"executable"`         | `0755`     | not applicable  | not enforceable     |
| `"private-executable"` | `0700`     | not applicable  | not enforceable     |
| `"read-only"`          | `0444`     | `0555`          | read-only attribute |

~~~~ toml
[[files."id_ed25519"]]
moniker = "posix"
path = "~/.ssh/id_ed25519"
mode = "private"
~~~~

The manifest is authoritative for declared modes: `dojang status` reports a
destination whose permissions do not satisfy its declaration, and
`dojang apply` reconciles the destination toward the declared mode even when
its contents are unchanged.  A declared mode is a desired final state rather
than a precondition, so applying updates to a `read-only` destination widens
it temporarily and narrows it back afterwards.  Windows can only enforce the
`read-only` distinction; other declarations produce a warning there instead
of silently claiming success.  Executable modes are meaningless for directory
routes and are rejected.

The `kind` field declares whether the destination is an ordinary copied
entry (`"copy"`, the default) or a **deployment link** (`"symlink"`): a
symbolic link at the destination pointing back at the absolute path of the
route's source file in the repository checkout.

~~~~ toml
[[files.".vimrc"]]
moniker = "posix"
path = "~/.vimrc"
kind = "symlink"
~~~~

A deployment link is a one-way projection of the repository source.
`dojang apply` creates a missing link, repairs one whose target no longer
matches the source (for example after the checkout moved), and refuses to
replace an existing regular file or directory unless `--force` is given.
`dojang reflect` never reflects a deployment link, even with `--force` or
when a path under a linked directory is named explicitly.  A symbolic link
carries no portable permissions of its own, so a `kind = "symlink"` branch
cannot declare a non-default `mode`.  Whether a link is created as a file
link or a directory link on Windows follows from the `files` or `dirs`
table declaring the route; creating symbolic links on Windows requires
Developer Mode or administrator privileges.

### Route codecs

A detailed branch can set `codec` to transform a regular source file before
deployment.  The string form names an unconfigured codec; the object form has
`name` and `config` fields.  The default `identity` codec preserves bytes, so
existing compact and detailed routes keep their previous behavior.  Codec
metadata requires `path`, and a deployment link accepts only `identity`.
The built-in `template` codec has no configuration and renders UTF-8 source
with manifest variables under `vars` and machine facts under `facts`.
`encrypted`, `encrypted-re-add`, and `secret-template` use a named
`codec-backends` declaration and require `mode = "private"` or
`mode = "private-executable"`.

See [route codecs] for configuration values, caching, dry-run behavior, and
reflection policies.

A route can be named like `home_directory` in the example above,
which actually refers to the path where the source of config files are located
within the repository.  If it says `home_directory`, it refers to
*home\_directory/* in the repository, which is the directory where the manifest
file is located.  So if your repository has files like the following:

 -  *dojang.manifest*
 -  *home\_directory/*
     -  *.bash\_profile*
     -  *.inputrc*

When applied to Linux, the files are copied to the following paths:

 -  */home/$USER/.bash\_profile*
 -  */home/$USER/.inputrc*

Similarly, on Windows, they would look like below:

 -  *C:\\Users\\%USERNAME%\\.bash\_profile*
 -  *C:\\USers\\%USERNAME%\\.inputrc*

In practice, more complex routing is possible, see the [relevant
documentation](routing.en.md) for a detailed explanation of routing.

[route codecs]: codecs.en.md


List of files to ignore
-----------------------

There are usually a lot of unrelated files in the target path,
and if you try to use the command `dojang status` while leaving them untouched,
the terminal will be full of unrelated changes, making it difficult to actually
use.  To avoid this, you can list files to ignore in the target path.

The files to ignore are listed inside the `ignores` section, whose name must
match the source directory you defined in the `dirs` section. On the other hand,
the files specified in the `files` section cannot be ignored as they are single
files.  Each item in the list is described with a glob-like pattern syntax.
E.g.:

~~~~ toml
[dirs.HOME]
posix = "$HOME"
windows = "$UserProfile"

[ignores]
HOME = [
  ".config",
  ".DS_Store",
]
~~~~

Note that even if a file is on your ignore list, it will not be ignored
if it has already been included in the repository.  You can also use this
behavior to manage only files that have been added to the repository,
while ignoring everything else (`*`).


Hooks
-----

**[Hooks](hooks.en.md)** run custom scripts before and after `apply`, `reflect`,
`diff`, `status`, `edit`, and `unmanage`.  Each command has a `pre-*` and
successful `post-*` event.  `apply` also retains its first-apply events.  Hooks
can run `always`, `once` per repository and machine, or `on-change` with an
explicit revision key.

~~~~ toml
[[hooks.pre-apply]]
command = "/bin/echo"
args = ["Applying config files..."]

[[hooks.post-apply]]
id = "reload-service"
policy = "on-change"
change-key = "service-config-v2"
command = "/usr/bin/systemctl"
args = ["--user", "restart", "my-service"]
when = "os = linux"
~~~~

For a detailed description of hooks, see the [hooks documentation](hooks.en.md).

<!-- cSpell:ignore inputrc -->
