File path expression
====================

A **file path expression** is a small syntax that allows you to construct
file paths with manifest variables and inherited environment variables.  The
syntax is similar to
[Bash's parameter expansion][1] syntax, so if you're familiar with it,
you'll be able to use it without having to learn it.
Here are some examples of various file path expressions.
One expression per line:

~~~~
/etc/foo
C:\Windows\system32
$UserProfile
$HOME/.config
${XDG_CONFIG_HOME:-$HOME/.config}
~~~~

[1]: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html


File paths
----------

Not every part of a file path expression needs to be a variable; it can mix
variables and plain file paths, or contain no variables at all.  Both
MS-DOS/Windows-style file paths and POSIX-style file paths are supported.
(However, even on Windows, we recommend using forward slashes instead of
backslashes.)


Variable lookup
---------------

Dojang first looks for a selected [manifest variable] with the referenced
name.  If the manifest has no matching declaration branch, it looks in the
inherited process environment.  Manifest-variable values are themselves file
path expressions, so references can be nested and declaration order does not
matter.  A selected empty value counts as defined and shadows an inherited
value.  Cycles among selected values are errors.

This lookup applies wherever Dojang consumes a file path expression, including
file and directory routes and hook `working-directory` fields.

[manifest variable]: manifest.en.md#manifest-variables


Variable substitution
---------------------

A file path may contain variables, or a file path expression may consist of
only one variable.  There are two syntaxes for variable substitution:

 -  `$ENV_VAR`: If the boundaries of the environment variable are obvious,
    you can use this syntax in most cases.
 -  `${ENV_VAR}`: Sometimes the boundaries of an environment variable are
    unclear, in which case you use this syntax.

Note that even on Windows, we don't use the `%ENV_VAR%` or `$env:ENV_VAR`
syntax, instead we use the POSIX shell-style syntax above.

To include a literal dollar sign, write `$$`.  For example, `$$HOME` expands
to the literal text `$HOME`, while `$HOME` looks up the variable named `HOME`.


Variable substitution with default
----------------------------------

If a variable is undefined or empty, you can tell it to use a given default
expression instead.  Simply write a default expression after `:-`.  A default
expression can include substitutions for other variables.
For example, the file path expression below uses the value of
the `XDG_CONFIG_HOME` environment variable if it exists,
but otherwise uses `$HOME/.config`:

~~~~
${XDG_CONFIG_HOME:-$HOME/.config}
~~~~


Conditional variable substitution
---------------------------------

You can make a given expression be used only when a variable is defined and
nonempty.  Write the conditional expression after `:+`; similarly, it can
include other variable substitutions.  For example, the file path expression
below takes the result of `$BAR\baz` only if `FOO` is defined and nonempty:

~~~~
${FOO:+$BAR\baz}
~~~~
