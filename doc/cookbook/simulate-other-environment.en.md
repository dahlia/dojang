Simulating other environment
============================

If you've modified your [manifest](../manifest.en.md) file and need to test
how it will behave in a different environment, you can use
the [*dojang-env.toml* file](../environment.en.md#overriding-environment)[^1]
with the `--dry-run` option.

The *dojang-env.toml* file tricks Dojang into recognizing the environment
written in that file instead of recognizing the actual environment.
(If it doesn't exist, it will recognize the actual environment.)
It has the same format as the output of the `dojang env` command,
so it's a good idea to have files of the `dojang env` results on
every device you own.

The `--dry-run` option causes the `dojang` command to simulate
any kind of filesystem changes it makes, completely in a sandbox.
Because it doesn't actually make any changes to the filesystem,
it's safe to run any commands with it.  So, with the `--dry-run` option,
you can see in advance what files Dojang will change and how.

Both features can be used with any subcommand of `dojang`.
Note that the `--dry-run` option is global and must be placed before
the subcommand, i.e. instead of writing something like:

~~~~ console
$ dojang apply --force --dry-run
~~~~

You should write something like this:

~~~~ console
dojang --dry-run apply --force
~~~~

[^1]: You can use `-e`/`--env-file` option to use a file other than
      *dojang-env.toml*.
