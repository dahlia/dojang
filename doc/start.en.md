Getting started
===============

Prior to starting
-----------------

Dojang is a cross-platform dotfile manager.  The word dotfile comes from
the fact that on POSIX-like platforms, programs place their config files,
which start with a period, in the home directory, but here we say dotfile to
refer to various forms of config files.  For example, *~/.vimrc* and
*~\\AppData\\Roaming\\alacritty\\alacritty.toml* could both be covered by
Dojang.

Dojang does not offer device-to-device sync of config files,
and has no plan to do so in the future. Instead, you can achieve
device-to-device sync by managing your own repository with a SCM like Git or
Mercurial, or by using cloud storage like Dropbox or iCloud.

Also, because this is a command-line program, you'll need to have basic
terminal skills. It does not provide a graphical user interface,
and there are no plan to do so in the future.


Installation
------------

Dojang is a program that hasn't been officially released yet,
so for now you'll have to build it to install it, which can take some time.
Please refer to the [installation](installation.en.md) docs to install Dojang.

Once the installation is complete, the `dojang` command will be available.
You can check how to use the `dojang` command with the `dojang --help` command.


Basic ideas
-----------

Dojang makes a distinction between **target** config files, which actually
apply to your device, and **source** config files, which are neutrally
structured to be usable across devices.  The directory that holds the latter is
called a **repository** in Dojang.

A repository contains a [**manifest**](manifest.en.md) file that describes
where the source config files should be copied to on the device,
which files and directories should be chosen based on the operating system, etc.
The manifest file is usually named *dojang.toml* and placed at the top of
the repository.

Actually syncing and applying source config files to a device is
called **application**, and conversely, when you change a target config file
that is being applied to a device, syncing it to the source config file is
called **reflection**.  Application and reflection can be done through
the `dojang` command.


Initializing the repository
---------------------------

Once you've created a repository, you'll be using it on all of your devices
in the future, so initializing it is something you'll only do once.

To initialize a repository, you first need an empty directory:

~~~~ console
$ mkdir my-dotfiles/
$ cd my-dotfiles/
~~~~

Feel free to name the directory anything you like.  Running the `dojang init`
command inside an empty directory will initialize the repository and
create a manifest file:

~~~~ console
$ dojang init
~~~~

When asked about the types of devices you use,
you can select everything you use.

> [!NOTE]
>
> On Windows, `dojang init` does not yet support interactive mode;
> this will be fixed in the future (see issue [#4]).  Until then,
> you can use the command line options to choose your device types.
> Try `dojang init --help` to see what options are available.
> For example, if you use both a Windows PC and an Intel Mac,
> you might type something like:
>
> ~~~~
> > dojang init --win64 --intel-mac
> ~~~~

You can now open the manifest file and edit it as you want.
For a detailed description of the manifest file,
see the [manifest](manifest.en.md) docs.

[#4]: https://github.com/dahlia/dojang/issues/4


Checking things out
-------------------

Once you've initialized your repository, you can check the current status
with the `dojang status` command.  This command compares the config files
on your device with the config files in the repository, showing you which
config files are being applied, which original config files should be applied,
and so on.

However, it can be overwhelming if there are too many irrelevant files
in the target directory, and the `dojang status` command can be slow
if there are too many files.  In this case, you can specify which files
to ignore in the `ignores` section.  For instance, the example below tells
it to ignore the *Documents* directory inside the home directory:

~~~~ toml
[ignores]
HOME = ["Documents"]
~~~~

For more information, see
[Ignoring irrelevant files](routing.en.md#ignoring-irrelevant-files).


Routing
-------

[**Routing**](routing.en.md) means copying config files to different locations
depending on your environment.  For example, the location of the home directory
is different on each operating system, so the config files in the home directory
need to be copied to a different location for each operating system.

It's no exaggeration to say that this routing is the most important thing
in the manifest file.  The routing is described in the `dirs` and `files`
sections of the manifest file.  When you initialize your repository with
the `dojang init` command, you should already have basic routing described in
your manifest.  For example, here is the routing for config files located in
your home directory:

~~~~ toml
[dirs.HOME]
posix = "$HOME"
windows = "$UserProfile"
~~~~

The `HOME` is a route name, which means that the config files in the directory
named *HOME/* in the repository will be the source.  The route name can be
any other name you want.

The `posix` and `windows` are monikers, defined in the `monikers` section of
the manifest file.  The monikers can be named anything other than `posix` or
`windows`.  Each moniker refers to criteria for the environment;
for example, the `posix` moniker might be defined as follows:

~~~~ toml
[monikers.posix]
os = ["linux", "macos"]  # Linux or macOS
~~~~

So, `posix` indicates if it is Linux or macOS. The route setup `posix = "$HOME"`
means to copy the config files inside the *HOME/* directory to the path pointed
to by the environment variable `HOME` if you are Linux or macOS.
Similarly, `windows = "$UserProfile"` means to copy the config files inside
the *HOME/* directory to the path pointed to by the environment variable
`UserProfile` on Windows.

For more information, see [routing](routing.en.md).


Importing existing config files
-------------------------------

You'll need to add your existing config files to the repository, for example,
the [*~/.inputrc*][inputrc] file.

You can just copy the config files you've used into your repository,
but it's better to use the `dojang reflect` command, which has safeguards
against mistakes.  In particular, `dojang reflect` can also be used to reflect
changes to the config files installed on your device when they are made.

~~~~ console
$ dojang reflect ~/.inputrc
~~~~

If the *~/.inputrc* file is on your ignore list, you might get an error like
the one below:

~~~~ console
$ dojang reflect ~/.inputrc
Error: File /home/USER/.bash_history is ignored due to pattern "*" (route name: HOME).
Hint: You can reflect it anyway by enforcing it using -f/--force option.
~~~~

In this case, you can force it to be reflected using the `-f` option.

[inputrc]: https://tiswww.case.edu/php/chet/readline/readline.html#Readline-Init-File

### Reflecting all changed files

You can also run `dojang reflect` without any arguments to reflect all changed
files at once.  This is useful when you've made changes to multiple config
files on your device and want to sync them all back to the repository.

~~~~ console
$ dojang reflect
Found 3 changed file(s):
  /home/user/.inputrc
  /home/user/.vimrc
  /home/user/.bashrc
Reflect all changed files? [y/N]
~~~~

The command will show a list of changed files and ask for confirmation before
proceeding.  If you want to skip the confirmation prompt, use the `--all`
(`-a`) flag:

~~~~ console
$ dojang reflect --all
~~~~

By default, files matching ignore patterns are skipped with a warning.
Use `--force` (`-f`) to include them.

### Reflecting unregistered files

If you have new files in your target directories that aren't yet tracked
by the repository, you can use the `--include-unregistered` (`-u`) flag
to include them:

~~~~ console
$ dojang reflect --include-unregistered
~~~~

When multiple routes could match an unregistered file, Dojang will prompt you
to select which route to use.


Editing config files
--------------------

When you want to edit a config file that is already being managed by Dojang,
you can use the `dojang edit` command.  This command opens the source file
in your editor and automatically applies changes after you save and close it.

~~~~ console
$ dojang edit ~/.inputrc
~~~~

The `dojang edit` command looks up the source file corresponding to the target
file (*~/.inputrc* in this case), opens it in your editor, and then runs
`dojang apply` after you finish editing.

By default, the editor is determined in the following order:

1.  The `--editor` (`-E`) option
2.  The `$VISUAL` environment variable
3.  The `$EDITOR` environment variable
4.  Platform default (`notepad` on Windows, `vi` on POSIX)

If you don't want to apply changes automatically, you can use the `--no-apply`
(`-n`) option.

> [!NOTE]
>
> The `dojang edit` command works from any directory because Dojang remembers
> the repository path in the *~/.dojang* file.  This file is automatically
> created when you run `dojang apply`.

### Editing all changed files

Similar to `dojang reflect`, you can run `dojang edit` without any arguments
to edit all changed source files at once:

~~~~ console
$ dojang edit
Found 2 changed file(s):
  HOME/.inputrc
  HOME/.vimrc
Edit all changed source files? [y/N]
~~~~

Use the `--all` (`-a`) flag to skip the confirmation prompt.
Use `--sequential` (`-s`) to edit files one at a time instead of opening
them all at once.

### Creating new config files with edit

You can also use `dojang edit` to create new config files.  If you specify
a target path that doesn't exist yet, Dojang will create an empty source file
and open it in your editor:

~~~~ console
$ dojang edit ~/.my-new-config
Creating new source file: HOME/.my-new-config
Opening HOME/.my-new-config in vi...
~~~~

If multiple routes could match the target path, Dojang will prompt you to
select which route to use.  After you save and close the editor,
`dojang apply` will copy the new file to the target location.


*.dojang* directory
-------------------

Dojang keeps internal records in the *.dojang* directory in order to do
three-way diffs between the config files applied to your device and the source
config files.  This directory is placed at the root of the repository.
The contents of this directory should not be modified or deleted by you,
and it should not be put on a SCM because its contents are device-specific.

Therefore, when using a SCM like Git or Mercurial, you should tell it to ignore
this directory.  If you're using Git, you can do this by adding it to your
*~/.gitignore* file like below:

~~~~ gitignore
.dojang/
~~~~

> [!TIP]
>
> If you ran the `git init` command before running the `dojang init` command,
> `dojang init` will automatically generate a *.gitignore* file.


Adding a new config file
------------------------

If you want to add an entirely new config file, you have two options:

1.  Use `dojang edit` with the target path (see
    [Creating new config files with edit](#creating-new-config-files-with-edit))
2.  Create the source config file directly in your repository

If you create the file directly in your repository, you'll need to use
the `dojang apply` command instead of `dojang reflect` because you'll need to
copy it from the repository to the target directory.

Let's say you want to add a new config file that will be located in
*~/.my-new-config*.  Create the *HOME/.my-new-config* file in your repository,
write the source config file, and check things out with the `dojang status`
command.  You should see a line like this within the results:

~~~~
Source ST Destination DT File
────── ── ─────────── ── ──────────────────────────────────────────
                          (… omitted …)
added  F  unchanged   -  .my-new-config
                          (… omitted …)
~~~~

The line has the following meanings, in order:

 -  `added`: The original config file has been added to the repository and has
    not yet been applied.
 -  `F`: the source config file is a regular file (not a directory or symbolic
    link).
 -  `unchanged`: The paired file in the target directory has not been changed
    or added.
 -  `-`: The paired file in the target directory does not yet exist.

Now that you know what's going on, let's apply it with
the `dojang apply` command.  Unlike `dojang reflect`, which takes file paths
as arguments and only copies the specified files, this command copies all
the files in the repository that don't already exist in the target directory
or need to be changed.


Applying to a new device
------------------------

The `dojang apply` command, which copies all files that need to be changed,
can also be used to apply config files to a new device at once thanks to
its behavior.  For example, you can put a repository up on Git,
clone it on your new device, and run the `dojang apply` command.

<!-- cSpell:ignore alacritty dotfile inputrc vimrc -->
