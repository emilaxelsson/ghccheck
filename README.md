# ghccheck

A wrapper around GHC to make it easier to use in an IDE compile loop.

Usage:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ghccheck [OPTIONS] ...  (urecognized options and files passed on to GHC)

  -? --help     Display help message
  -V --version  Print version information
  -n --no-conf  Don't read a configuration file (.ghci or $HOME/.ghccheck)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When calling `ghccheck ARGS`, it will in turn call GHC with something like

    ghc -O0 --make -no-link -hidir .ghc-temp -odir .ghc-temp GHCI-OPTS ARGS

where `GHCI-OPTS` is a list of options found in a GHCi configuration file and `ARGS` is the arguments passed to `ghccheck`, *except* those flags that are recognized by `ghccheck` itself (see above).

`ghccheck` will look for the GHCi configuration in the following locations in order:

  * `.ghci` (in the directory where `ghccheck` is called)
  * `$HOME/.ghccheck`

Any line beginning with `:set ` in the GHCi configuration is recognized as an option to pass to GHC.

