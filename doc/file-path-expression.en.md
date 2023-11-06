File path expression
====================

A **file path expression** is a small syntax that allows you to construct
file paths with environment variables.  The syntax is similar to
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

Not every part of a file path expression needs to be an environment variable;
it can be a mix of environment variables and plain file paths,
or no environment variables at all.  Both MS-DOS/Windows-style file paths and
POSIX-style file paths are supported.  (However, even on Windows, we recommend
using forward slashes instead of backslashes.)


Environment variable substitution
---------------------------------

A file path may contain environment variables, or a file path expression
may consist of only one environment variable.
There are two syntaxes for environment variable substitution:

 -   `$ENV_VAR`: If the boundaries of the environment variable are obvious,
     you can use this syntax in most cases.
 -   `${ENV_VAR}`: Sometimes the boundaries of an environment variable are
     unclear, in which case you use this syntax.

Note that even on Windows, we don't use the `%ENV_VAR%` or `$env:ENV_VAR`
syntax, instead we use the POSIX shell-style syntax above.


Environment variable substitution with default
----------------------------------------------

If an environment variable is undefined, you can tell it to use a given default
expression instead.  Simply write a default expression followed by `:-`.
a default expression can include substitutions for other environment variables.
For example, the file path expression below uses the value of
the `XDG_CONFIG_HOME` environment variable if it exists,
but otherwise uses `$HOME/.config`:

~~~~
${XDG_CONFIG_HOME:-$HOME/.config}
~~~~


Conditional environment variable substitution
---------------------------------------------

You can make a given expression be used only when an environment variable is
defined.  The expression to be used conditionally can be followed by `:+`,
and similarly, the expression can include other environment variable
substitutions.  For example, the file path expression below takes the result of
the expression `$BAR\baz` only if the `FOO` environment variable is defined:

~~~~
${FOO:+$BAR\baz}
~~~~
