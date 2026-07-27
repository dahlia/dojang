Installation
============

This article explains how to install Dojang.


Verified release installer
--------------------------

The release installer downloads the executable and the release's
*SHA256SUMS* file.  It verifies the selected archive before installing
anything.

On Linux or macOS, download the installer into a fresh temporary directory:

~~~~ bash
installer_directory="$(
  mktemp -d "${TMPDIR:-/tmp}/dojang-install.XXXXXX"
)"
if test -z "$installer_directory" ||
  ! curl -fsSLo "$installer_directory/install.sh" \
    https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.sh
then
  test -z "$installer_directory" || rm -rf "$installer_directory"
  installer_directory=
fi
~~~~

Inspect the downloaded script:

~~~~ bash
test -n "$installer_directory" &&
  less "$installer_directory/install.sh"
~~~~

After inspection, run it:

~~~~ bash
test -n "$installer_directory" &&
  sh "$installer_directory/install.sh"
~~~~

The POSIX installer supports x86-64 and AArch64.  It installs to
*~/.local/bin* by default.

On x86-64 Windows, download the installer in PowerShell:

~~~~ powershell
$installerDirectory = Join-Path ([IO.Path]::GetTempPath()) ([Guid]::NewGuid())
$installer = Join-Path $installerDirectory install.ps1
New-Item -ItemType Directory -Path $installerDirectory | Out-Null
try {
  Invoke-WebRequest `
    https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.ps1 `
    -OutFile $installer `
    -UseBasicParsing `
    -ErrorAction Stop
} catch {
  Remove-Item $installerDirectory -Recurse -Force -ErrorAction Ignore
  $installer = $null
  throw
}
~~~~

Inspect the downloaded script:

~~~~ powershell
if ($installer) {
  Get-Content $installer
}
~~~~

After inspection, run it:

~~~~ powershell
if ($installer) {
  Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force
  & $installer
}
~~~~

Set `DOJANG_INSTALL_DIR` to choose another directory.  To install a specific
release instead of the latest one, set `DOJANG_INSTALL_VERSION`, for example
to `0.3.0`, before running the script.

Set `DOJANG_INSTALL_DOWNLOAD_DIR` to an explicit directory to download and
verify the selected archive without extracting or installing it.  The
directory receives both the archive and the release's *SHA256SUMS* file.  For
example, after downloading and inspecting `install.sh` as above:

~~~~ bash
test -n "$installer_directory" &&
  DOJANG_INSTALL_DOWNLOAD_DIR="$PWD/dojang-download" \
    sh "$installer_directory/install.sh"
~~~~

On Windows, after downloading and inspecting `install.ps1` as above, set the
same environment variable and then run the local script:

~~~~ powershell
if ($installer) {
  $env:DOJANG_INSTALL_DOWNLOAD_DIR = "$PWD\dojang-download"
  Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force
  & $installer
}
~~~~


Homebrew (macOS & Linux)
------------------------

Dojang can be automatically installed via [Homebrew] by downloading its
executable.  Enter the following command in the terminal:

~~~~ bash
brew tap dahlia/dojang https://github.com/dahlia/dojang.git
brew install --cask dahlia/dojang/dojang
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

~~~~ bash
scoop bucket add dojang https://github.com/dahlia/dojang.git
scoop install dojang
~~~~

[Scoop]: https://scoop.sh/


mise (cross-platform)
---------------------

If you use [mise] for managing development tools, Dojang can be installed via
the GitHub backend.  Enter the following command in the terminal:

~~~~ bash
mise use -g github:dahlia/dojang
~~~~

[mise]: https://mise.jdx.dev/


Container image and static Linux executable
-------------------------------------------

Linux releases also publish a multi-architecture container image for x86-64
and AArch64.  To check a tagged image:

~~~~ bash
docker run --rm ghcr.io/dahlia/dojang:0.3.0 version
~~~~

The image contains a statically linked executable.  You can extract it on a
Linux machine with the same architecture as the image:

~~~~ bash
docker run --rm --entrypoint cat ghcr.io/dahlia/dojang:0.3.0 \
  /usr/local/bin/dojang > dojang
chmod +x dojang
~~~~


Build it manually
-----------------

Dojang is a program made in Haskell, so you need to install the
[Haskell Tool Stack].  Please refer to the [installation guide][1] on the Stack
official website to install the Stack.  If you can use the `stack` command in
the terminal, the installation is complete.

Now you are ready to build Dojang.  Enter the following command in the terminal:

~~~~ bash
git clone https://github.com/dahlia/dojang.git
cd dojang/
stack build
stack install
~~~~

The `stack install` command installs the `dojang` executable in
the *~/.local/bin* directory.  You can now use the `dojang` command.

If you want to install it in a different directory, use the `stack install`
command with the `--local-bin-path` option. For example, the following command
will install the `dojang` executable in the *~/bin* directory:

~~~~ bash
stack install --local-bin-path ~/bin
~~~~

[Haskell Tool Stack]: https://haskellstack.org/
[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
