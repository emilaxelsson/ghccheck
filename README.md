# ghccheck



`ghccheck` is a wrapper around GHC to make it easier to use in an IDE compile loop -- that is, in situations where we are not interested in the resulting binaries but only in warnings and errors. (Although `ghccheck` does keep intermediate GHC files in the `.ghc-temp` directory in order to avoid unnecessary recompilation.)



Usage:
----------------------------------------------------------------------------------------------------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ghccheck [OPTIONS] ...  (urecognized options and files passed on to GHC)

  -? --help         Display help message
  -V --version      Print version information
  -n --no-conf      Don't read a configuration file (.ghci or $HOME/.ghccheck)
  -i --interactive  Interactive mode (uses GHCi)
  -r --run          Run the main definition in the program (à la runghc)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When calling `ghccheck ARG1 ARG2 ...`, it will call GHC with something like

    ghc --make -O0 -no-link -hidir .ghc-temp -odir .ghc-temp GHCI-OPTS ARG1 ARG2 ...

where `GHCI-OPTS` is a list of options found in a GHCi configuration file (see below) and `ARG1 ARG2 ...` are the arguments (options or files) passed to `ghccheck`, *except* those flags that are recognized by `ghccheck` itself (see above).



Configuration
----------------------------------------------------------------------------------------------------

`ghccheck` will look for the GHCi configuration in the following files in order:

  * `.ghci` (in the directory where `ghccheck` is called)
  * `$HOME/.ghccheck`

Any line beginning with `:set ` in the GHCi configuration files is recognized as an option to pass to GHC. Also `:script <filename>` instructions are recognized and result in the file `filename` to be expanded into the configuration file.

In order to ignore the configuration file, use the option `-n`/`--no-conf`.



Cabal sandbox integration
----------------------------------------------------------------------------------------------------

In order to use `ghccheck` in a Cabal sandbox, just call it as follows:

    cabal exec -- ghccheck ARGS

(This will make `ghc` aware of the sandbox location, but it will not get any other things, such as language extensions, from the Cabal package.)



Limitations
----------------------------------------------------------------------------------------------------

Since `ghccheck` calls `ghc --make`, it will assume that any module named `Main` (i.e. also modules without any module header) contains a `main` definition. I have not been able to work around this.

The solution is to

  * always declare a module header so that modules are not implicitly named `Main`, or
  * make sure to include a `main` definition in the module.

