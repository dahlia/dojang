Dealing with conflicts
======================

Many programs don't just read config files, they also modify them.
Sometimes, instead of fixing the source config file, a user might forget
and fix the destination config file.  If the user realizes this quickly,
they can immediately reflect that change to the source with
the `dojang reflect` command, but if they forget and make another change
to the source settings, a conflict can occur.

A conflict exists when the source and destination have both changed from their
common intermediate snapshot and the resulting states are incompatible.  Both
`dojang apply` and `dojang reflect` stop before changing the filesystem when
they detect such a conflict, unless you use `-f`/`--force`.

A change only on the side that the command will overwrite is not a conflict.
`dojang apply` treats the source as authoritative, while `dojang reflect`
treats the destination as authoritative.  Running `dojang reflect` can
therefore replace a source-only change with the destination version without
`--force`.

To resolve a conflict, first determine which config file is causing it.  Run
`dojang status` to find the file.  For example, you might see output like
below:

~~~~ console
$ dojang status
Source   ST Destination DT File
──────── ── ─────────── ── ──────────────────
modified F  modified    F  HOME/.bash_profile
~~~~

The above results mean the following, in order:

 -  `modified`: The source config file has been changed since the last time
    it was applied (`dojang apply`).
 -  `F`: The source config file is a regular file (not a directory or symlink).
 -  `modified`: The destination config file has been changed since the last time
    it was applied.
 -  `F`: The destination config file is a regular file (not a directory or
    symbolic link).
 -  `HOME/.bash_profile`: The source path of the conflicting config file.

Once you've identified the conflicting config file, you need to see specifically
what part of it is causing the conflict.  You can run the `dojang diff` command
to see the changes in the config file that are causing the conflict.
For example, you might see results like the following:

~~~~ console
$ dojang diff
--- ./HOME/.bash_profile
+++ /home/dahlia/.bash_profile
@@ -1,3 +1,3 @@
-# This line is changed from the source.
+# This line is changed from the destination.

~~~~

> [!TIP]
>
> The output format of the `dojang diff` command is basically equivalent to one
> of the `diff --unified` commands.  If you want a different output format,
> use the `--diff-program` option.  For example, the `--diff-program delta`
> option allows you to change to the output format of the [`delta`][delta]
> program.

Once you've identified the changes to the config file that are causing
the conflict, it's time to resolve the conflict.  There are three ways to
resolve conflicts.

 -  Discard the changes in the destination file and only take the changes in
    the source file: You can do this by running the `dojang apply -f` command.
 -  Discard the changes in the source file and only take the changes in
    the destination file: Run the `dojang reflect -f` command.
 -  Make changes in both the source and destination files: You can directly
    modify the source config file that causes the conflict,
    and run the `dojang apply -f` command.

[delta]: https://github.com/dandavison/delta
