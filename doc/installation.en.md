Installation
============

This article explains how to install Dojang.


Verified release installer
--------------------------

The release installer downloads the executable and the release's
*SHA256SUMS* file.  It verifies the selected archive before installing
anything.

On Linux or macOS:

~~~~ console
$ curl -fsSL https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.sh | sh
~~~~

The POSIX installer supports x86-64 and AArch64.  It installs to
*~/.local/bin* by default.

On x86-64 Windows, run this in PowerShell:

~~~~ powershell
irm https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.ps1 | iex
~~~~

Set `DOJANG_INSTALL_DIR` to choose another directory.  To install a specific
release instead of the latest one, set `DOJANG_INSTALL_VERSION`, for example
to `0.3.0`, before running the script.

Set `DOJANG_INSTALL_DOWNLOAD_DIR` to an explicit directory to download and
verify the selected archive without extracting or installing it.  The
directory receives both the archive and the release's *SHA256SUMS* file.  For
example, the following manual workflow avoids piping a network response
directly into a shell:

~~~~ console
$ curl -fsSLO https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.sh
$ less install.sh
$ DOJANG_INSTALL_DOWNLOAD_DIR="$PWD/dojang-download" sh ./install.sh
~~~~

On Windows, download and inspect `install.ps1`, set the same environment
variable, and then run the local script:

~~~~ powershell
Invoke-WebRequest `
  https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.ps1 `
  -OutFile install.ps1
Get-Content .\install.ps1
$env:DOJANG_INSTALL_DOWNLOAD_DIR = "$PWD\dojang-download"
.\install.ps1
~~~~


Homebrew (macOS & Linux)
------------------------

Dojang can be automatically installed via [Homebrew] by downloading its
executable.  Enter the following command in the terminal:

~~~~ console
$ brew tap dahlia/dojang https://github.com/dahlia/dojang.git
$ brew install --cask dahlia/dojang/dojang
~~~~

> [!TIP]
>
> If you want to build it manually instead of downloading the executable,
> use the `--formula` option instead of the `--cask` option.

[Homebrew]: https://brew.sh/


Scoop (Windows)
---------------

Dojang can be automatically installed via [Scoop] by downloading its executable.
Enter the following command in the terminal:

~~~~ console
$ scoop bucket add dojang https://github.com/dahlia/dojang.git
$ scoop install dojang
~~~~

[Scoop]: https://scoop.sh/


mise (cross-platform)
---------------------

If you use [mise] for managing development tools, Dojang can be installed via
the GitHub backend.  Enter the following command in the terminal:

~~~~ console
$ mise use -g github:dahlia/dojang
~~~~

[mise]: https://mise.jdx.dev/


Container image and static Linux executable
-------------------------------------------

Linux releases also publish a multi-architecture container image for x86-64
and AArch64.  To check a tagged image:

~~~~ console
$ docker run --rm ghcr.io/dahlia/dojang:0.3.0 version
~~~~

The image contains a statically linked executable.  You can extract it on a
Linux machine with the same architecture as the image:

~~~~ console
$ docker run --rm --entrypoint cat ghcr.io/dahlia/dojang:0.3.0 \
>   /usr/local/bin/dojang > dojang
$ chmod +x dojang
~~~~


Build it manually
-----------------

Dojang is a program made in Haskell, so you need to install the
[Haskell Tool Stack].  Please refer to the [installation guide][1] on the Stack
official website to install the Stack.  If you can use the `stack` command in
the terminal, the installation is complete.

Now you are ready to build Dojang.  Enter the following command in the terminal:

~~~~ console
$ git clone https://github.com/dahlia/dojang.git
$ cd dojang/
$ stack build
$ stack install
~~~~

The `stack install` command installs the `dojang` executable in
the *~/.local/bin* directory.  You can now use the `dojang` command.

If you want to install it in a different directory, use the `stack install`
command with the `--local-bin-path` option. For example, the following command
will install the `dojang` executable in the *~/bin* directory:

~~~~ console
$ stack install --local-bin-path ~/bin
~~~~

[Haskell Tool Stack]: https://haskellstack.org/
[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
