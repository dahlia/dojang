Environment
===========

> [!NOTE]
>
> An **environment** is not the same thing as an **environment variable**.

An **environment** is a set of basic attributes to distinguish between
different devices to which config files will be applied,
currently consisting of three things: **operating system** (`os`), **processor
architecture** (`arch`), and **kernel** (`kernel`).[^1]  For example,
a typical Windows PC probably has the following environment:

~~~~ toml
os = "windows"
arch = "x86_64" # a 64-bit CPU from the Intel or AMD family

[kernel]
name = "Microsoft Windows"
release = "10.0.23585.1001"  # Windows 11 22H2
~~~~

Or, if you have a MacBook bought in 2022 or later, you probably have something
like below:

~~~~ toml
os = "macos"
arch = "aarch64" # Apple silicon

[kernel]
name = "Darwin"
release = "23.1.0"  # macOS 14.1
~~~~

[^1]: More may be added in the future.  For example, it could contain
      the version of the operating system.


Check your current environment
------------------------------

Before you start using Dojang, you can find out what environment it recognizes
your device as by running the `dojang env` command.

~~~~ console
$ dojang env
os = "linux"
arch = "x86_64"

[kernel]
name = "Linux"
release = "5.10.0-8"
~~~~


List of recognized operating systems
------------------------------------

Here is a list of operating systems recognized by Dojang and the keywords that
refer to them in Dojang:[^2]

 -  Android: `android`
 -  FreeBSD: `freebsd`
 -  GNU/Linux: `linux`
 -  macOS (all versions of Mac OS X and later): `macos`
 -  NetBSD: `netbsd`
 -  OpenBSD: `openbsd`
 -  Windows: `windows`

[^2]: More may be added in the future.  You can even add one, if you really
      want to, by contributing to the Dojang project.


List of recognized processor architectures
------------------------------------------

The following is a list of processor architectures recognized by Dojang and
the keywords that refer to them in Dojang:[^2].

- ARM64: `aarch64`
- Intel/AMD 32-bit: `x86`
- Intel/AMD 64-bit: `x86_64`


Kernel recognition
------------------

Dojang recognizes the kernel of the device it's running on.  The recognized
kernel is derived from `uname -sr` for POSIX systems and `ver` for Windows.


Overriding environment
----------------------

Dojang recognizes the environment of the device it's running on for you,
but you may want to pretend it's something other than the recognized one,
especially when you want to test how it will behave on different devices.
By placing a *dojang-env.toml* file[^1] next to the *dojang.toml* file,
you can make Dojang see the environment specified in that file instead of
the environment it recognizes by itself.  The file will be formatted as
the same as the result of `dojang env`:

~~~~ toml
os = "linux"
arch = "aarch64"

[kernel]
name = "Linux"
release = "6.5.9-300.fc35.aarch64"
~~~~

For your information, you can also use command `dojang env -o dojang-env.toml`
to create a sample of *dojang-env.toml* file, and then work from there.

[^1]: You can use `-e`/`--env-file` option to use a file other than
      *dojang-env.toml*.
