Machine facts
=============

Machine facts are named, case-insensitive text values that let one repository
distinguish machines which otherwise have the same operating system,
architecture, and kernel.  For example, a repository can route a file only on
a work machine:

~~~~ toml
[monikers.work]
when = 'fact.class = "work"'
~~~~

Facts are repository-scoped configuration, not secrets.  Their names and
values can appear in diagnostics, machine state, route provenance, and hook
fingerprints.  Do not store passwords, tokens, or other credentials in them.


Keys and values
---------------

A fact key consists of one or more dot-separated segments.  Every segment must
start with an ASCII letter and may continue with ASCII letters, digits,
hyphens, or underscores.  Keys such as `class`, `org.team`, and
`hardware.gpu-model` are valid.  Keys and values compare case-insensitively,
while their original spelling is retained when written.

Dojang detects these built-in facts:

 -  `os`: operating system
 -  `arch`: processor architecture
 -  `kernel`: kernel name
 -  `kernel-release`: kernel release
 -  `hostname`: network hostname

The existing `os`, `arch`, `kernel`, and `kernel-release` predicates remain
available.  Their generic equivalents, such as `fact.os = linux`, have the
same matching and specificity behavior.  Built-in values cannot be persisted
in a `[facts]` table or with `--fact`; use an environment simulation when a
test needs to replace one.


Declaring facts
---------------

Put reusable facts in the `[facts]` table of a TOML file:

~~~~ toml
[facts]
class = "work"
"org.team" = "platform"
~~~~

Run `dojang init` in an existing repository to enroll the current machine.
The command examines route and hook predicates, follows referenced monikers,
and prompts for user-defined facts that have no value.  It does not prompt for
facts in branches eliminated by predicate normalization.

For scripts, CI, Windows, or any other noninteractive use, supply the values
explicitly:

~~~~ console
$ dojang init --no-interactive --fact class=work
$ dojang init --no-interactive --facts-file ~/.config/dojang/work.toml
~~~~

`--fact` stores a repository-specific value in machine state.  Repeating the
option is allowed, and the last value for a key wins.  `--facts-file` instead
associates the repository with the file, so multiple repositories can reuse
one shared profile without copying its values.  A file inside the checkout is
stored as a relative path and follows a moved checkout.  A file outside the
checkout is stored as an absolute path and must remain at that location.

When *dojang-env.toml* beside the manifest already has a nonempty `[facts]`
table, `dojang init` associates it automatically.  An explicit `--facts-file`
is still clearer for a shared profile.  `--dry-run` may validate and report the
enrollment, but it does not store answers or the file association.

Noninteractive enrollment exits with code 22 and lists every missing key.  An
interactive prompt is also unavailable on Windows, so pass `--fact` or
`--facts-file` there.  Running `dojang forget` removes the repository's fact
values and file association along with its other machine-local state.


Precedence and simulation
-------------------------

Dojang assembles the environment in this order, with later sources winning
for user-defined keys:

1.  Detected built-ins, including the hostname
2.  The associated facts file
3.  Repository-specific values stored by `--fact` or an interactive prompt
4.  The environment simulation selected by `-e`/`--env-file`

The last source is deliberately a testing override.  A complete environment
file must still contain `os`, `arch`, and `[kernel]`; it may also specify a
top-level `hostname` and a `[facts]` table:

~~~~ toml
os = "linux"
arch = "aarch64"
hostname = "test-host"

[kernel]
name = "Linux"
release = "6.12.0"

[facts]
class = "test"
~~~~

Use `--ignore-env-file` with `dojang env` to inspect only detected host
information.  The hostname is exposed to hook processes as `DOJANG_HOSTNAME`;
custom facts participate in `on-change` fingerprints but are not exported as
individual environment variables.


Using facts in predicates
-------------------------

The canonical spelling is `fact.` followed by the fact key:

~~~~ text
fact.class = work
fact.org.team != personal
fact.hardware.gpu in (integrated, discrete)
fact.location not in (home, travel)
~~~~

Generic facts support `=`, `!=`, `in`, and `not in`.  Prefix and suffix
comparisons remain limited to `kernel-release`.  A predicate that refers to an
undefined fact evaluates to false and reports a warning.

See [environment predicates] for composition with `&&`, `||`, `!`, and
monikers.

[environment predicates]: environment-predicate.en.md
