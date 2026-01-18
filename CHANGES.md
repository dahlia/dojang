Dojang changelog
=================

Version 0.2.0
-------------

To be released.

 -  The `dojang init` command now generates *.gitignore* if the source directory
    is a Git repository and the file does not exist yet.  [#16]

 -  Fixed a bug that Dojang had not recognized `kernel.release` field in
    a *dojang-env.toml* file unlike how it was documented and how `dojang env`
    command have behaved.  [#17]

 -  Added disambiguation for ambiguous source paths in the `dojang reflect`
    command.  When multiple routes map to the same destination, Dojang now:

     -  Auto-selects the route if only one source file exists
     -  Shows an interactive prompt with full source paths and `(exists)` labels
     -  Warns when multiple source files exist (potential configuration issue)
     -  Supports `DOJANG_AUTO_SELECT` environment variable (`first`, `error`)
     -  Accepts `--source` option to explicitly specify which source to use

    [[#22]]

 -  Added the `dojang edit` command which opens the source file of a target
    file in the user's editor and applies changes after editing.  [[#21]]

     -  Editor detection priority: `--editor` option > `$VISUAL` > `$EDITOR`
        > platform default (`notepad` on Windows, `vi` on POSIX)
     -  Use `--no-apply` flag to skip automatic apply after editing
     -  Use `--force` flag to skip conflict warnings
     -  Use `--sequential` flag to edit files one at a time
     -  Use `--source` option or `DOJANG_AUTO_SELECT` to resolve
        ambiguous routes

 -  Added a registry file (*~/.dojang*) that stores the repository path.
    This allows the `dojang edit` command to work from any directory.  [[#21]]

     -  Created automatically during `dojang apply`
     -  If the registry points to a different repository, Dojang prompts
        to overwrite (in interactive mode) or overwrites silently
        (in non-interactive mode)

[#16]: https://github.com/dahlia/dojang/issues/16
[#17]: https://github.com/dahlia/dojang/issues/17
[#21]: https://github.com/dahlia/dojang/issues/21
[#22]: https://github.com/dahlia/dojang/issues/22


Version 0.1.0
-------------

Initial release.  Released on November 26, 2023.
