Installation
============

Dojang has never been officially released yet, so you'll need to build it to
install it. This article explains how to install Dojang.


Homebrew (macOS & Linux)
------------------------

Dojang can be automatically built and installed via [Homebrew].
Enter the following command in the terminal:

~~~~ console
$ brew tap dahlia/dojang https://github.com/dahlia/dojang.git
$ brew install --HEAD dahlia/dojang/dojang
~~~~

[Homebrew]: https://brew.sh/


Build it manually
-----------------

Dojang is a program made in Haskell, so you need to install the [Haskell Tool
Stack].  Please refer to the [installation guide][1] on the Stack official
website to install the Stack.  If you can use the `stack` command in
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
