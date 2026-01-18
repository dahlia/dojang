Hooks
=====

**Hooks** allow you to run custom scripts before and after the `dojang apply`
command.  This is useful for tasks such as:

 -  Installing dependencies before applying config files
 -  Reloading services after config changes
 -  Running setup scripts on first-time installation
 -  Cleaning up temporary files


Hook types
----------

There are four types of hooks, executed in the following order:

| Hook type          | When it runs                          |
| ------------------ | ------------------------------------- |
| `pre-apply`        | Before every apply, before file sync  |
| `pre-first-apply`  | Only on first apply, before file sync |
| `post-first-apply` | Only on first apply, after file sync  |
| `post-apply`       | After every apply, after file sync    |

**First apply** is determined by the absence of the registry file (*~/.dojang*).
When you run `dojang apply` for the first time, all four hooks run in this
order:

1.  `pre-apply`
2.  `pre-first-apply`
3.  *(file synchronization)*
4.  `post-first-apply`
5.  `post-apply`

On subsequent applies, only `pre-apply` and `post-apply` run:

1.  `pre-apply`
2.  *(file synchronization)*
3.  `post-apply`


Defining hooks
--------------

Hooks are defined in the `hooks` section of your *dojang.toml* manifest file.
Each hook type uses TOML's array of tables syntax (`[[hooks.TYPE]]`):

~~~~ toml
[[hooks.pre-apply]]
command = "/bin/echo"
args = ["Setting up..."]

[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "restart", "my-service"]
~~~~

You can define multiple hooks of the same type.  They will be executed in the
order they are defined.

### Hook fields

 -  `command` (string, required): The path to the executable to run.
    This must be an absolute path or a command available in `$PATH`.

 -  `args` (array of strings, optional): Arguments to pass to the command.
    Defaults to an empty array.

 -  `when` (string, optional): An
    [environment predicate](environment-predicate.en.md) expression.  The hook
    only runs if the condition is satisfied. Defaults to always run.

 -  `working-directory` (string, optional): The working directory for
    the hook.  Defaults to the repository directory.

 -  `ignore-failure` (boolean, optional): If `true`, Dojang continues
    even if the hook exits with a non-zero status.  Defaults to `false`.

### Conditional hooks

You can use the `when` field to run hooks only on certain environments:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "restart", "dunst"]
when = "os = linux"

[[hooks.post-apply]]
command = "/usr/bin/killall"
args = ["NotificationCenter"]
when = "os = macos"
~~~~


Environment variables
---------------------

The following environment variables are available to hooks:

| Variable            | Description                                   |
| ------------------- | --------------------------------------------- |
| `DOJANG_REPOSITORY` | Absolute path to the repository directory     |
| `DOJANG_MANIFEST`   | Path to the manifest file (*dojang.toml*)     |
| `DOJANG_DRY_RUN`    | `1` if running in dry-run mode, `0` otherwise |
| `DOJANG_OS`         | Current operating system identifier           |
| `DOJANG_ARCH`       | Current processor architecture identifier     |


Dry-run mode
------------

When running `dojang apply --dry-run`, hooks are not executed.  Instead,
Dojang prints what would be run:

~~~~
Note: Would run hook: /bin/echo Setting up...
~~~~


Error handling
--------------

If a hook exits with a non-zero status, Dojang stops and exits with
exit code 40 (`hookFailedError`).  Use `ignore-failure = true` to continue
despite hook failures:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/optional-command"
args = []
ignore-failure = true
~~~~


Examples
--------

### First-time setup

Run a setup script only on first apply:

~~~~ toml
[[hooks.pre-first-apply]]
command = "/bin/bash"
args = ["-c", "echo 'First time setup!' && ./setup.sh"]
~~~~

### Platform-specific hooks

Run different commands based on the operating system:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/brew"
args = ["bundle", "--file=Brewfile"]
when = "os = macos"

[[hooks.post-apply]]
command = "/usr/bin/apt"
args = ["install", "-y", "packages..."]
when = "os = linux"
~~~~

### Service reload

Reload a service after config changes:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "reload", "my-app"]
when = "os = linux"
~~~~

<!-- cSpell:ignore dunst killall Brewfile -->
