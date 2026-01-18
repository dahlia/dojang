Routing
=======

It is not uncommon for the same config file to be placed in different places
depending on the environment.  For example, the config file for `psql`,
a client of PostgreSQL, has the following different paths depending
on the environment, according to [documentation][1]:

  - POSIX: If the `PSQLRC` environment variable is present,
    use its value as the path; if not, use *~/.psqlrc*.
  - Windows: If the `PSQLRC` environment variable is present,
    use its value as the path;
    otherwise, use *%APPDATA%\postgresql\psqlrc.conf*.

Dojang utilizes a concept called **routing** to deal with such complex cases.

[1]: https://www.postgresql.org/docs/current/app-psql.html#APP-PSQL-FILES-PSQLRC


Directory Routing
-----------------

Many programs store their config files in `$XDG_CONFIG_HOME` on Linux,
*~/Library/Application Support* on macOS, and `%AppData%` or `%LocalAppData%`
on Windows.[^1]  So if you want to use the same config file in multiple
environments, it's convenient to route those paths together.
The declaration below shows an example of this.

~~~~ toml
[dirs.app_config]
linux = "$XDG_CONFIG_HOME"
mac = "$HOME/Library/Application Support"
win = "$AppData"

[monikers.linux]
os = "linux"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

Here, the routing name, `app_config`, is also the name of a directory within
the repository. If we say that the files in the repository are as follows:

- *dojang.toml*
- *app_config/*
    - *alacritty/*
        - *alacritty.toml*
    - *ghc/*
        - *ghci.conf*
    - *nvim/*
        *init.vim*

On Linux, you will have files in the following paths:

- */home/$USER/.config/alacritty/*
- */home/$USER/.config/alacritty/alacritty.toml*
- */home/$USER/.config/ghc/*
- */home/$USER/.config/ghc/ghci.conf*
- */home/$USER/.config/nvim/*
- */home/$USER/.config/nvim/init.vim*

However, what if the `XDG_CONFIG_HOME` environment variable is not defined?
The `app_config` routing on Linux would be an empty path, i.e. the current
working directory (CWD) at the time you run Dojang.  If you want to prevent
this, you can tweak the routing a bit, as shown below.

~~~~ toml
linux = "${XDG_CONFIG_HOME:-$HOME/.config}"
~~~~

The syntax above means that if the `XDG_CONFIG_HOME` environment variable
exists, take its value, but otherwise take the path *~/.config*.
For more advanced features like this, see the [file path
expressions](file-path-expression.en.md) documentation.

[^1]: For example, in the cross-platform app framework Electron,
      the `app.getPath("appData")` API returns different paths depending on
      the environment.  In the same manner, GLib's `get_user_config_dir()` API
      behaves similarly.

### Ignoring irrelevant files

In some cases, like your home directory, where you might have a lot of stuff
besides your config file, the results of `dojang status` can be overwhelming
and difficult to see.  In addition, too many files can make it slow.

To avoid this, you can specify which files to ignore in the `ignores` section.
For example, suppose you added a `home` route like below:

~~~~ toml
[dirs.home]
linux = "$HOME"
mac = "$HOME"
win = "$UserProfile"
~~~~

Now, any files inside the directory that the `home` routes to will appear in
the results of `dojang status`.  However, you may want to ignore the *Documents*
directory among them.  In this case, you can specify the files you want to
ignore in the `ignores` section, as shown below:

~~~~ toml
[ignores]
home = [
  "Documents",
]
~~~~

In practice, there are much more files in the home directory to ignore than not,
so it's usually more convenient to just ignore everything:

~~~~ toml
[ignores]
home = ["*"]
~~~~

Even if we ignore all the files like this, we can still reflect them by
specifying them one by one with the `dojang reflect -f` command.
Once added to the repository, files are managed even if they are on
the ignore list.

### In case of overlapping routing

Let's say you have routed `app_config` to `XDG_CONFIG_HOME` on Linux,
which is *~/.config*, as in the example above, and you want to add a route to
your home directory:

~~~~ toml
[dirs.home]
linux = "$HOME"
mac = "$HOME"
win = "$UserProfile"
~~~~

In most cases, the directory where `home` is routed will also contain
the directory where `app_config` is routed.  It's easy to make mistakes
when dealing with overlapping directories in two or more routes like this,
so you should have one side ignore the other, like below:

~~~~ toml
[ignores].
home = [
  ".config",
  "AppData/Roaming",
  "Library/Application Support"
]
~~~~


Per-file routing
----------------

For some config files, not only the directory but also the filename
itself can vary depending on the environment, and it can be simpler to route
them on a per-file basis.  For example, the `psql` config file,
which we used as an example at the beginning of this document,
could be routed on a per-file basis, as shown below:

~~~~ toml
[files.".psqlrc"]
posix = "${PSQLRC:-$HOME/.psqlrc}"
win = "${PSQLRC:-${AppData:-$UserProfile/AppData/Roaming}/postgresql/psqlrc.conf}"

[monikers.posix]
os = ["linux", "macos"]

[monikers.win]
os = "windows"
~~~~


Route priority
--------------

When a single file or directory has multiple routes that could match
the current environment, Dojang determines which route to use based on
**specificity**.  The more specific a predicate is, the higher its priority.
This ensures that specialized configurations take precedence over
general ones.

### How specificity is calculated

Specificity is measured by how many conditions must be satisfied for a
predicate to match.  Here are the key rules:

 -  **Single conditions** (like `os = linux` or `arch = "x86_64"`) have
    a specificity of 1.

 -  **And (`&&`)** combines the specificities of its parts.  For example,
    `os = linux && arch = "x86_64"` has a specificity of 2 because
    both conditions must be satisfied.

 -  **Or (`||`)** takes the maximum specificity among its parts.
    For example, `os = linux || os = macos` has a specificity of 1
    because only one condition needs to match.

 -  **Monikers** inherit the specificity of the predicate they resolve to.
    A moniker defined as `os = linux && arch = "x86_64"` has the same
    specificity as writing that expression directly.

 -  **`always`** has a specificity of 0, meaning it matches everything but
    with the lowest priority.

### Examples

Consider a manifest with the following routes for a config file:

~~~~ toml
[files.".bashrc"]
apple-silicon = "~/Library/bashrc"
posix = "$HOME/.bashrc"
linux = "$HOME/.bashrc.linux"

[monikers.apple-silicon]
when = "os = macos && arch = aarch64"

[monikers.posix]
when = "os in (linux, macos)"

[monikers.linux]
when = "os = linux"
~~~~

In this configuration:

 -  `apple-silicon` has specificity 2 (two conditions with `&&`)
 -  `posix` has specificity 1 (single `in` check)
 -  `linux` has specificity 1 (single `=` check)

On an Apple Silicon Mac (macOS with aarch64 architecture), both
`apple-silicon` and `posix` would match.  However, `apple-silicon` wins
because it has higher specificity (2 > 1).

On an Intel Mac (macOS with x86_64 architecture), only `posix` matches.

On Linux, both `posix` and `linux` match with equal specificity.
In this case, the route order in the manifest becomes the tiebreaker—whichever
appears first in the predicate comparison takes priority.

### Practical tips

 -  **Start general, add specific**: Define broad routes first (like `posix`
    for Unix-like systems), then add more specific ones (like `linux` or
    `apple-silicon`) that override for particular environments.

 -  **Use `&&` for specialization**: When you need environment-specific
    behavior, combine conditions with `&&` to create more specific predicates
    that take priority.

 -  **Avoid overlapping `||` routes**: Routes using `||` don't increase
    specificity much.  If you find yourself with ambiguous matches,
    consider restructuring with `&&` conditions instead.


## Null routing

Some config files may not exist or be needed in some environments.
In these cases, you can choose not to route them at all,
which is called **null routing**.  For example, if you have a config file
that is only needed on Windows, you can route it like below:

~~~~ toml
[files."Microsoft.WindowsTerminal.json"]
win = "$LocalAppData/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"
~~~~

The above config file is only required on Windows, so you won't have it at all
on Linux or macOS.

<!-- cSpell: ignore alacritty APPDATA ghci nvim psql PSQLRC 8wekyb3d8bbwe -->
