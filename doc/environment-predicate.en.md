Environment predicate
=====================

An **environment predicate** is a small grammar that describes conditions on
an [environment](environment.en.md).  In a [manifest](manifest.en.md) file,
environment predicates can be used in the `when` field of the `moniker` section.
Here are some examples of different environment predicates.
One predicate per line:

~~~~
os = linux
arch = "x86_64"
os in (linux, freebsd, macos)
arch not in (x86, "x86_64")
kernel-release ^= "4.1"
!(os in (linux, freebsd, macos) && arch = "x86_64" || os = openbsd)
~~~~

Left-hand side
--------------

There are three fields that are subject to predication:

 -  `os`: operating system
 -  `arch`: processor architecture
 -  `kernel`: kernel
 -  `kernel-release`: kernel version
 -  `moniker`: moniker


Comparison operators
--------------------

There are four operators that can be used to compare against a target of
a predicate:

- `=`: is …
- `!=`: is not …
- `^=`: starts with … (only available for `kernel-release` field)
- `$=`: ends with … (only available for `kernel-release` field)
- `in`: is one of …
- not in`: not any of …

Comparison operators are always preceded by fields, followed by string literals
after `=`/`!=`/`^=`/`$=`, and lists after `in` and `not in`.

> **Note**
>
> All comparison operators are case-insensitive.


Right-hand side
---------------

### String literals

String literals are representations of string values, such as `linux` and
`"macos"`, and are of three types:

 -  Bare string literals: These are not enclosed in any quotation marks and
    start with an alphabet, and can only contain alphabets and numbers.
    Most values can be represented this way, such as `linux`, `windows`,
    and `aarch64`.
 -  Double-quoted string literal: A string enclosed in double quotes, supporting
    C/Python-style escape-column syntax (e.g., `\0`, `\n`, `\"`, `\xff`,
    `\uffff`).  Double-quotes can be written literally without escape sequences.
-   Single-quote string literals: strings enclosed in single-quotes, supporting
    C/Python-style escape-column syntax (e.g. `\0`, `\n`, `\'`, `\xff`,
    `\uffff`).  Single-quotes can be written literally without escape sequences.

In most cases, bare string literals will be sufficient,
except when representing values like `"x86_64"`.

### Lists

A list is an expression that contains zero or more string literals,
and is a comma-separated list of string literals in parentheses.
For example, `()` is an empty list, and `("x86_64", aarch64)` is a list of two
strings.


Boolean constants
-----------------

Although you'll never actually use them, there are two keywords for boolean
constants:

 -  `always`: true
 -  `never`: false

These boolean constants are never used with comparison operators,
and always stand alone as predicates.


Parentheses
-----------

Predicates can be enclosed in parentheses to give them operational precedence.


Boolean operators
-----------------

There are three types of boolean operators:

 -  `&&`: An infix binary operator that requires both predicates to be true,
    otherwise the whole predicate is false.
 -  `||`: An infix binary operator, when only one of the predicates on either
    side is true, the whole predicate is true, otherwise false.
 -  `!`: A prefix unary operator, used immediately before an opening
    parenthesis, that negates the predicate inside the parenthesis.
