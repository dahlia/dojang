Hooks
=====

Hooks run commands around the lifecycles of `apply`, `reflect`, `diff`,
`status`, `edit`, and `unmanage`.  They can prepare tools before a command,
reload a service after a successful change, or perform setup once on each
machine.


Lifecycle events
----------------

Each supported command has a `pre-*` and `post-*` event.  Post-hooks run only
when the command succeeds.

| Command    | Before event   | Successful-post event |
| ---------- | -------------- | --------------------- |
| `apply`    | `pre-apply`    | `post-apply`          |
| `reflect`  | `pre-reflect`  | `post-reflect`        |
| `diff`     | `pre-diff`     | `post-diff`           |
| `status`   | `pre-status`   | `post-status`         |
| `edit`     | `pre-edit`     | `post-edit`           |
| `unmanage` | `pre-unmanage` | `post-unmanage`       |

After their pre-hooks, `reflect`, `diff`, `status`, `edit`, and `unmanage`
reload *dojang.toml* and *dojang-env.toml* for the command action.  They reload
the same context again before selecting successful post-hooks, so those hooks
cannot run from an older manifest or machine environment.

`apply` also supports `pre-first-apply` and `post-first-apply`.  On the first
successful apply for a repository and machine, the order is:

1.  `pre-apply`
2.  `pre-first-apply`
3.  File synchronization
4.  `post-first-apply`
5.  `post-apply`

A failed or dry-run apply does not consume first-apply state.  Moving the
checkout does not reset it.  `dojang forget` does.

Hooks do not run around `init`, `migrate`, `env`, or `forget`.


Defining hooks
--------------

Define hooks in the `hooks` section of *dojang.toml*.  A lifecycle event uses
TOML array-of-tables syntax:

~~~~ toml
[[hooks.pre-reflect]]
command = "/usr/bin/make"
args = ["prepare"]

[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "reload", "my-service"]
when = "os = linux"
~~~~

Hooks for one event run in manifest order.  Stateful hook IDs must be unique
within an event.

### Fields

 -  `command` (string, required): executable path or command found through
    `PATH`.
 -  `args` (array of strings): command arguments.  The default is an empty
    array.
 -  `when` (string): [environment predicate] that must match before the hook
    is considered.  The default is `always`.
 -  `working-directory` (string): [file path expression] for the working
    directory.  Relative results use the repository directory as their base.
    The default is the repository directory.
 -  `ignore-failure` (boolean): continue the command after a launch error or
    nonzero exit.  The default is `false`.
 -  `id` (string): stable identity for a stateful hook.  It must match
    `[A-Za-z][A-Za-z0-9._-]*`.
 -  `policy` (string): `always`, `once`, or `on-change`.  The default is
    `always`.
 -  `change-key` (nonempty string): explicit revision key required by
    `on-change`.

[environment predicate]: environment-predicate.en.md
[file path expression]: file-path-expression.en.md


Execution policies
------------------

`always` runs whenever the event occurs and the `when` predicate matches.  An
`id` is optional, and successful runs are not stored.

`once` requires `id`.  It runs until it succeeds once for this repository and
machine:

~~~~ toml
[[hooks.pre-status]]
id = "install-status-helper"
policy = "once"
command = "/usr/local/bin/install-status-helper"
~~~~

`on-change` requires both `id` and `change-key`.  It runs after the explicit
key, normalized hook configuration, matching machine facts, lifecycle event,
selected path scope, resolved working directory, or an input used to derive
that directory changes:

~~~~ toml
[[hooks.post-apply]]
id = "rebuild-cache"
policy = "on-change"
change-key = "cache-format-v3"
command = "/usr/local/bin/rebuild-cache"
~~~~

The fingerprint does not read managed file contents or hash the full manifest.
Change `change-key` when a script's behavior changes in a way that Dojang
cannot observe.  History is repository-scoped and machine-local, so moving a
checkout does not make a successful `once` hook run again.

Only successful executions are recorded.  A failure allowed by
`ignore-failure = true` continues the command but remains due next time.


Hook environment
----------------

Hooks inherit the parent process environment.  Dojang overlays these values:

| Variable                    | Value                             |
| --------------------------- | --------------------------------- |
| `DOJANG_REPOSITORY`         | Absolute repository path          |
| `DOJANG_MANIFEST`           | Absolute manifest path            |
| `DOJANG_REPOSITORY_ID`      | Repository UUID                   |
| `DOJANG_MACHINE_STATE`      | Repository machine-state file     |
| `DOJANG_INTERMEDIATE`       | Intermediate snapshot path        |
| `DOJANG_COMMAND`            | Command name                      |
| `DOJANG_HOOK_EVENT`         | Lifecycle event                   |
| `DOJANG_HOOK_ID`            | Stable or derived hook identity   |
| `DOJANG_HOOK_POLICY`        | Execution policy                  |
| `DOJANG_DRY_RUN`            | `0` for an executed hook          |
| `DOJANG_OS` / `DOJANG_ARCH` | Operating system and architecture |
| `DOJANG_KERNEL`             | Kernel name                       |
| `DOJANG_KERNEL_RELEASE`     | Kernel release                    |
| `DOJANG_HOSTNAME`           | Hostname, when detected           |
| `DOJANG_PATH_COUNT`         | Number of selected scope paths    |
| `DOJANG_PATH_0`, …          | Scope paths in stable order       |

Route selectors passed through `reflect --source`, `edit --source`, and
`unmanage --route` remain relative to the repository.  Other relative path
arguments use the caller's working directory.  Selecting a repository with
`-r` therefore does not change the scope or fingerprint of a route selector.
Custom [machine facts] affect `on-change` fingerprints but are not exported as
individual environment variables.

Post-hooks also receive `DOJANG_COMMAND_OUTCOME=success` and
`DOJANG_EXIT_CODE=0`.  Before adding the current context, Dojang removes
inherited hook-context variables.  Values omitted for the current event, such
as post-command results on a pre-hook or path indexes beyond
`DOJANG_PATH_COUNT`, therefore remain unset.

[machine facts]: machine-facts.en.md


Dry-run and recursion
---------------------

Dry-run mode prints each due hook with its lifecycle event, effective working
directory, and policy reason.  It does not start a process, acquire a hook lock,
or update execution history.

A Dojang process started by a hook suppresses its own hooks by default.  Pass
`--allow-hook-recursion` to the outer invocation to allow hooks in one nested
invocation.  Recursion identities include the repository ID, so an identically
named hook in another repository may run.  A second nested level is still
suppressed, and an ancestor hook in the same repository cannot re-enter itself.


Failures and concurrency
------------------------

A launch error or nonzero exit stops Dojang with exit code 40
(`hookFailedError`) unless `ignore-failure = true`.  Asynchronous interruption
is not treated as an ignored hook failure.

Stateful hooks with the same repository, event, and `id` are serialized across
processes.  Eligibility is checked again while holding the hook lock, then a
successful execution is saved through the repository's atomic state update.
If the repository is forgotten and recreated while a hook is running, the old
execution cannot enter the new state generation.  Hook execution identities
and policy data use validated types before they reach the state writer.

<!-- cSpell:ignore systemctl -->
