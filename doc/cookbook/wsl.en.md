Detecting WSL
=============

> [!TIP]
>
> WSL, which stands for Windows Subsystem for Linux, is a feature that allows
> running Linux on Windows.
> See the [official WSL docs](https://docs.microsoft.com/windows/wsl/)
> for more information.

> [!NOTE]
>
> This document assumes WSL 2.  It may not work on WSL 1.
> (Contributions welcome, though.)

WSL is detected as a generic Linux environment in Dojang.  However,
if you have config files that you want to treat specially only in WSL,
you need to be able to distinguish WSL from a generic Linux environment.

Currently, the only way to detect WSL seems to be to check if the output of
`uname -r` or the contents of */proc/version* contains the string
`-microsoft-standard-WSL2`.[^1]  Fortunately, Dojang recognizes the `kernel-release`
field when detecting the environment, so you can define
a [moniker](../manifest.en.md#moniker) that is satisfied only in WSL in
your [manifest](../manifest.en.md) file as follows:

~~~~ toml
[monikers.wsl]
when = "kernel-release $= '-microsoft-standard-WSL2'"
~~~~

Note that you need to use the [environment predicate
syntax](../environment-predicate.en.md) in the `when` field
because you need to use the `$=` (ends with) operator instead of
a normal string equality comparison.

[^1]: [How to detect if running under WSL?][1]

[1]: https://github.com/microsoft/WSL/issues/4071
