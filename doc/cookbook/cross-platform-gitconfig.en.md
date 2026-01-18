Cross-platform *.gitconfig*
===========================

Dojang allows you to use nearly the same Git settings across devices,
while still making some settings different depending on your environment.
For example, you can use the same name or aliases for your favorite commands
on all your devices, but have different settings for the editor, SSH,
and GnuPG programs on different devices.

Suppose you want to use the following settings on macOS:

~~~~ gitconfig
[user]
	name = John Doe
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
[core]
	editor = vim
[gpg]
	path = /usr/local/bin/gpg
~~~~

… and want to use the following settings on Windows:

~~~~ gitconfig
[user]
	name = John Doe
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
[core]
	editor = notepad
	sshCommand = C:/Windows/System32/OpenSSH/ssh.exe
	autocrlf = false
	symlinks = true
[credential "helperselector"]
	selected = manager
[gpg]
	program = C:\\Program Files (x86)\\GnuPG\\bin\\gpg.exe
~~~~

How do we achieve this?


Let the contents vary depending on the environment
--------------------------------------------------

We'll work on sharing common settings in one place later, but first we need
to make them environment-specific. To do this, we need to make sure that
the environment-specific configuration files are copied to *~/.gitconfig* via
per-file routing.  In your [manifest](../manifest.en.md) (*dojang.toml*) file,
put the following settings:

~~~~ toml
[files."gitconfig/.gitconfig.mac"]
mac = "$HOME/.gitconfig"

[files."gitconfig/.gitconfig.win"]
win = "$UserProfile/.gitconfig"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

Notice that we've created one per-file routing for one environment.
If you look at the routing in *gitconfig/.gitconfig.mac*,
it only contains the path when the `mac` moniker is satisfied,
and the path when the `win` moniker is satisfied is missing.
Intentionally omitting certain environments like this is called
[null routing](../routing.en.md#null-routing).  With null routing,
routing is considered to not exist for certain environments,
and the configuration file for those environments is not copied.

With these settings, the contents of *gitconfig/.gitconfig.mac* will be copied
to *~/.gitconfig* on macOS, and the contents of *gitconfig/.gitconfig.win*
will be copied to *~/.gitconfig* on Windows.


Create a common configuration file
----------------------------------

Putting common settings in one place is a simple matter of using the `[include]`
function supported by Git config itself.  Create *gitconfig/.gitconfig.mac* like
below:

~~~~ gitconfig
[include]
	path = ~/.gitconfig.common
[core]
	editor = vim
[gpg]
	path = /usr/local/bin/gpg
~~~~

The *gitconfig/.gitconfig.win* will look like this

~~~~ gitconfig
[include]
	path = ~/.gitconfig.common
[core]
	editor = notepad
	sshCommand = C:/Windows/System32/OpenSSH/ssh.exe
	autocrlf = false
	symlinks = true
[credential "helperselector"]
	selected = manager
[gpg]
	program = C:\\Program Files (x86)\\GnuPG\\bin\\gpg.exe
~~~~

Finally, create a common config file, *gitconfig/.gitconfig.common*:

~~~~ gitconfig
[user]
	name = John Doe
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
~~~~

Don't forget to add one more per-file routing to the manifest file so that
the *gitconfig/.gitconfig.common* file is copied to *~/.gitconfig.common*,
as shown below:

~~~~ toml
[files."gitconfig/.gitconfig.common"]
mac = "$HOME/.gitconfig.common"
win = "$UserProfile/.gitconfig.common"
~~~~

<!-- cSpell:ignore autocrlf helperselector -->
