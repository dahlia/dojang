Manifest
========

A **manifest** file, named *dojang.toml*, declares that the directory in which
it resides is a repository for config files managed by Dojang, and sets up how
those config files should be applied on the actual machine.

It is in [TOML] format, as you might guess from the extension, and is divided
into four main sections: `dirs`, `files`, `monikers`, and `ignores`.
This document assumes that the reader knows the basic syntax of TOML.

[TOML]: https://toml.io/


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

A route can be named like `home_directory` in the example above,
which actually refers to the path where the source of config files are located
within the repository.  If it says `home_directory`, it refers to
*home_directory/* in the repository, which is the directory where the manifest
file is located.  So if your repository has files like the following:

- *dojang.manifest*
- *home_directory/*
    - *.bash_profile*
    - *.inputrc*

When applied to Linux, the files are copied to the following paths:

- */home/$USER/.bash_profile*
- */home/$USER/.inputrc*

Similarly, on Windows, they would look like below:

- *C:\Users\\%USERNAME%\\.bash_profile*
- *C:\USers\\%USERNAME%\\.inputrc*

In practice, more complex routing is possible, see the [relevant
documentation](routing.en.md) for a detailed explanation of routing.


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


<!-- cSpell:ignore inputrc -->
