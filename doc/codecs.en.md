Route codecs
============

A route codec transforms a repository file before Dojang writes it to the
intermediate snapshot and destination.  The repository keeps the raw source;
the intermediate snapshot keeps the rendered bytes.

Dojang includes five codecs.  `identity` copies bytes without changing them and
remains the default for every existing manifest.  `template` renders a
deterministic text template.  The sensitive codecs `encrypted`,
`encrypted-re-add`, and `secret-template` call a manifest-declared backend
through a narrow binary protocol.  Other names require an implementation
registered by an embedding application.


Manifest syntax
---------------

Only a detailed route branch can declare `codec`.  A string selects a codec
with empty configuration:

~~~~ toml
[[files."git/config"]]
moniker = "posix"
path = "~/.gitconfig"
codec = "identity"
~~~~

An object supplies normalized configuration.  The parser accepts an inline
object, and the manifest writer uses the equivalent nested-table form:

~~~~ toml
[[files."app/config"]]
when = "os = linux"
path = "~/.config/app/config"

[files."app/config".codec]
name = "example"

[files."app/config".codec.config]
format = "toml"
strict = true
~~~~

Configuration values may be strings, integers, booleans, arrays, or nested
tables.  Floating-point numbers and date or time values are rejected so the
same configuration has one deterministic identity.

A codec requires `path` and cannot be attached to a null route.  A
`kind = "symlink"` branch accepts only the default `identity` codec.
Deployment links remain a direct, one-way projection of the repository source.


Template codec
--------------

Select the built-in template codec with its string form.  It has no
configuration:

~~~~ toml
[vars]
git_name = "Ada"

[[files."git/config"]]
when = "always"
path = "~/.gitconfig"
codec = "template"
~~~~

The source file is UTF-8 text.  It can read exact, case-sensitive manifest
variable names through `vars` and case-insensitive machine-fact names through
`facts`:

~~~~ jinja
[user]
    name = {{ vars.git_name }}
{% if facts.os == "linux" %}
[credential]
    helper = libsecret
{% endif %}
~~~~

`vars` contains only active declarations from the manifest.  It never falls
back to the process environment.  A missing value is an error only if template
execution reaches the expression that reads it, so an unselected conditional
branch may refer to an unavailable value.  Errors report a one-based source
line and column without printing the source or rendered contents.  A
missing-value error retains its namespace, such as `vars.git_name` or
`facts.os`.  When the missing member name is computed dynamically, the error
uses `<dynamic>` instead of exposing the runtime-derived key.  If a selected
manifest variable is not valid platform text, the error identifies its
`vars` name without printing its value or an unrelated template position.

The supported language is a pure subset of Ginger/Jinja syntax:

 -  literals, comments, interpolation, lists, and dictionaries;
 -  `if`/`elif`/`else`, `switch`, finite `for` loops, local `set`, `scope`, and
    `indent` statements;
 -  indexing, ternaries, arithmetic, comparisons, Boolean operations, and
    deterministic text, numeric, collection, and predicate built-ins.

Local names become available only after their `set` statement.  After a
conditional, a newly assigned name is available only when every possible
branch assigns it.

The codec rejects includes, imports, inheritance, blocks, macros, calls with a
dynamic target, scripts in every Ginger whitespace-control and comment-prefix
form, `do`, lambdas, exception handling, and effectful or
representation-dependent built-ins such as `eval`, `throw`, date, regular
expression, escaping, JSON, and higher-order functions.  It preserves mixed
line endings and a trailing newline.  Operations that can raise an unchecked
Ginger runtime error are also unsupported: `center`, `divisibleby`, integer
division (`//` or `int_ratio`), modulo (`%` or `modulo`), printf-style
formatting (`format` or `printf`), and text replacement (`replace`).  Template
reflection is rejected even with `--force`; edit the repository template
instead.


Sensitive codec backends
------------------------

Declare a reusable backend in the manifest, then refer to its key from a
sensitive codec:

~~~~ toml
[codec-backends.age]
command = "$HOME/.local/bin/dojang-age-backend"
version = "age-1.2-profile-3"
timeout-seconds = 30
options = { identity = "work" }
~~~~

The command path must expand to an absolute path.  Dojang starts it directly,
without a shell or arguments, with an empty environment and the repository
root as its working directory.  Backend declarations are unavailable on
Windows until their process and owner-only storage behavior can be verified
there.

Standard input starts with one UTF-8 JSON line.  It identifies protocol
`dojang-codec-backend-v1`, the manifest-local backend, declared version,
operation, and non-secret options.  A lookup also has an `item` field.  The
newline is followed by the exact binary payload: encrypted source bytes for
`decrypt`, deployed bytes for `encrypt`, and an empty payload for `lookup`.
The backend writes only the requested bytes to standard output and exits zero.
On failure it exits nonzero and may write one JSON object to standard error
whose `code` is `missing-item`, `invalid-input`, `permission-denied`, or
`unavailable`.  Dojang discards all other backend output and reports only an
allowlisted message.

The default timeout is 30 seconds; the manifest can select 1 through 300
seconds.  Timeout, interruption, start failure, malformed diagnostics, and
nonzero exit all fail codec evaluation before synchronization begins.  The
backend receives secrets only through standard input and returns them only on
standard output.  It is still responsible for avoiding its own logs, crash
reports, and temporary files.


Encrypted codecs
----------------

`encrypted` stores ciphertext in the repository and deploys the backend's
decrypted bytes.  Its reflection policy is `reject`.  A sensitive route must
opt into an owner-only mode:

~~~~ toml
[[files."ssh/id_ed25519"]]
when = "always"
path = "~/.ssh/id_ed25519"
mode = "private"

[files."ssh/id_ed25519".codec]
name = "encrypted"

[files."ssh/id_ed25519".codec.config]
backend = "age"
~~~~

`encrypted-re-add` uses the same configuration but opts into reflection.  It
sends changed destination bytes to the backend's `encrypt` operation, then
decrypts the candidate ciphertext and accepts it only if the result exactly
matches the destination.  This works with randomized encryption: source delta
is based on the semantic decrypted bytes, not ciphertext equality.  Use the
default `encrypted` codec unless destination-to-repository reflection is a
deliberate part of the workflow.


Secret-template codec
---------------------

`secret-template` extends the deterministic template language with a static
two-argument function:

~~~~ jinja
token = {{ secret("vault", "services/example/token") }}
~~~~

Both arguments must be string literals.  The first names a `codec-backends`
entry; the second is the backend's item key.  The template stores references,
not returned values, in the repository.  Lookups are demand-driven, so an
unselected conditional branch does not contact its backend.  Repeated use of
one reference during an evaluation reuses the in-memory result.  Reflection is
always rejected.

The route must declare `mode = "private"` or `mode = "private-executable"`.
The latter is intended only for deployed files that must be executable.


Sensitive-data boundary
-----------------------

Encrypted repository bytes and secret references may be shared.  Decrypted
bytes and looked-up values exist in process memory and are written to the
intermediate snapshot and destination.  Dojang requires an explicit owner-only
route mode.  It writes new intermediate and destination content through
owner-only temporary files before publishing it and stores managed target
baselines below an owner-only transaction directory.  Sensitive codec results
and dependency metadata are command-scoped rather than written to the ordinary
codec cache.  Built-in diffs suppress sensitive bodies, and external diff
programs cannot inspect them.

These permissions are not encryption at rest.  A privileged account, a
misconfigured backup, swap, a filesystem snapshot, or a backend that writes a
temporary file can still retain plaintext.  Choose the destination and Dojang
state locations accordingly.  Protect the backend executable and its key
material separately.

`--dry-run` never contacts a sensitive backend, even if an old persistent
codec entry exists.  It reports that the codec needs a valid result instead of
pretending to know the planned bytes.  Run the command normally only when
online evaluation is intended.

Backend failure happens before source, intermediate, destination, or machine
state mutation.  After a timeout or failure, fix backend access and rerun the
same command.  Do not copy the destination plaintext over encrypted source.
If plaintext may have escaped through a backend, backup, or crash report,
rotate the affected secret and re-encrypt the repository source before the
next apply.


Evaluation and caching
----------------------

Dojang evaluates only routes selected by the command.  A codec receives the
raw source bytes, normalized configuration, and only the machine facts,
manifest variables, or controlled external inputs that it declared.  Every
selected non-identity route has its codec registration and configuration
validated even when its source is missing.  Analysis that depends on source
contents runs only for a regular source file.  Every
used input contributes a fingerprint to a deterministic cache key.  Unrelated
facts and variables do not invalidate the result.  Manifest variable values
for custom codecs retain their native operating-system bytes, including
non-UTF-8 values on POSIX.  The template codec decodes selected values as
strict platform text.  Machine facts use the environment predicate keys `os`,
`arch`,
`kernel`, `kernel-release`, and `hostname`.  A custom fact uses the part after
`fact.` in its predicate name.

Pure codec transformations receive the route configuration after it passes the
codec's validator.  They cannot use the application monad, filesystem,
terminal, or process APIs.  Effectful codec programs can request only external
inputs interpreted by the runtime; the built-in sensitive codecs use that
boundary for backend operations.

`dojang apply` compares rendered bytes with the intermediate snapshot and the
concrete destination.  It evaluates once while planning, writes that exact
result to the intermediate snapshot, and then deploys the snapshot.
`dojang status`, `dojang diff`, `dojang reflect`, and `dojang edit` use the same
rendered source view.  One command reuses an evaluation while its raw source is
stable.  The evaluation freezes the selected facts, variables, and external
inputs for that command.  Reflection uses the same inputs for reverse
transformation and round-trip validation.  Before executing the plan and again
before each buffered codec write, Dojang verifies that the authoritative file
still has the bytes used for evaluation.  A mismatch aborts the write.  The
built-in diff does not expose raw source bytes and reports non-UTF-8 output as a
binary difference.  Sensitive codec bodies are always suppressed.  An external
diff program is rejected when rendered or sensitive content would otherwise
have to be exposed to it.

Machine-state schema version 6 can store the codec name and version, a
configuration digest, the cache key, and dependency fingerprints.  It does not
store raw source bytes or a second copy of rendered bytes.  A cached result is
reused only when its key matches and the intermediate snapshot still matches
the recorded file fingerprint.  Before publishing convergence, Dojang also
checks that the raw source still matches the bytes used for evaluation.

A codec declares whether an uncached evaluation is safe during `--dry-run`.
Pure codecs may run.  A cache-only codec fails the dry run if no valid cached
result exists, so a simulation never claims an output it did not compute.  If
the codec declares an external input, the dry run fails before resolving it;
Dojang cannot validate an external-input fingerprint without invoking the
resolver.  A cache-only `re-add` codec also rejects dry-run reflection because
reversing and validating new deployed bytes is not covered by the forward
cache.


Reflection
----------

Reflection behavior belongs to the registered codec implementation, not to
the manifest:

 -  `identity` copies deployed bytes back to the repository.
 -  `reject` stops reflection before any selected file is changed.  `--force`
    does not bypass this policy.
 -  `re-add` reverses deployed bytes into repository bytes.  Dojang immediately
    runs the forward transformation on that result and accepts it only when it
    reproduces the deployed bytes exactly.  A missing destination removes the
    repository file.

A file route using a codec other than the built-in `identity` rejects a
directory or symbolic-link destination before any selected file changes.  This
type check applies to every reflection policy.

Codec failures identify the route, codec, and failure category.  Diagnostics,
debug output, and reconciliation plans redact raw and rendered byte contents.
