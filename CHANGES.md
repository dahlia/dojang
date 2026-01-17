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

[#16]: https://github.com/dahlia/dojang/issues/16
[#17]: https://github.com/dahlia/dojang/issues/17
[#22]: https://github.com/dahlia/dojang/issues/22


Version 0.1.0
-------------

Initial release.  Released on November 26, 2023.
