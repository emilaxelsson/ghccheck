module Main where



import Control.Exception
import Data.Monoid
import Data.Version
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Paths_ghccheck



-- | Command line options
data Options = Options
    { helpOpt        :: Bool
    , versionOpt     :: Bool
    , noConfOpt      :: Bool
    , interactiveOpt :: Bool
    , runOpt         :: Bool
    }
  deriving (Show)

defaultOptions = Options False False False False False

confFiles = "(.ghci or $HOME/.ghccheck)"

-- | Description of command line options
argMode :: Mode Options
argMode = mode
    "ghccheck"
    defaultOptions
    ("The ghccheck program v" ++ showVersion version)
    (flagArg (\_ _ -> Right defaultOptions)
      "...  (urecognized options and files passed on to GHC)")
    [ flagHelpSimple (\o -> o {helpOpt = True})
    , flagVersion (\o -> o {versionOpt = True})
    , flagNone
        ["n","no-conf"]
        (\o -> o {noConfOpt = True})
        ("Don't read a configuration file " ++ confFiles)
    , flagNone
        ["i", "interactive"]
        (\o -> o {interactiveOpt = True})
        ("Interactive mode (uses GHCi)")
    , flagNone
        ["r", "run"]
        (\o -> o {runOpt = True})
        ("Run the main definition in the program (à la runghc)")
    ]

-- | All flags (including dashes) recognized by ghccheck
flags :: [String]
flags = map dash $ concatMap flagNames $ modeFlags argMode
  where
    dash f@[_] = '-':f
    dash f     = "--" ++ f

-- | Read the arguments (obtained by `getArgs`). The second result contains all
-- unrecognized arguments. The third result contains anything that comes after
-- a plain "--" (including that empty flag).
readArgs :: [String] -> (Options, [String], String)
readArgs args
    | interactiveOpt opts && runOpt opts =
        error "ghccheck: cannot use both --interactive and --run"
    | otherwise = (opts, otherOpts, unwords rest)
  where
    (args',rest) = break (=="--") args
    otherOpts = filter (`notElem` flags) args'
    opts = Options
      { helpOpt        = "-?" `elem` args || "--help"        `elem` args
      , versionOpt     = "-V" `elem` args || "--version"     `elem` args
      , noConfOpt      = "-n" `elem` args || "--no-conf"     `elem` args
      , interactiveOpt = "-i" `elem` args || "--interactive" `elem` args
      , runOpt         = "-r" `elem` args || "--run"         `elem` args
      }

-- | Extract all options from a @.ghci@ file. Options are lines beginning with
-- @:set @.
listGOpts :: Text -> [Text]
listGOpts = filter (/="") . map getOpt . Text.lines
  where
    getOpt l = case Text.splitAt 5 l of
        (":set ",opt) -> opt
        _ -> ""

-- | Read a configuration file and expand any @:script@ commands
readConfFile :: FilePath -> IO Text
readConfFile = fmap Text.unlines . readConf
  where
    readConf :: FilePath -> IO [Text]
    readConf file = do
        conf <- Text.readFile file
        fmap concat $ mapM expand $ Text.lines conf

    expand :: Text -> IO [Text]
    expand l = case Text.splitAt 8 l of
        (":script ",file) ->
          readConf (Text.unpack file) `catch` \(_ :: IOException) -> do
            putStrLn $ "Warning: Imported script "
                    ++ Text.unpack file
                    ++ " not found; ignored"
            return []
        _ -> return [l]

-- Unfortunately, I wasn't able to use `processArgs` to parse the command line.
-- The problem is that I wasn't able to allow arbitrary additional options to be
-- passed on to GHC. This is why I use `fmap readArgs getArgs`.

main = do
    (opts,otherOpts,rest) <- fmap readArgs getArgs
    if helpOpt opts
      then print argMode
    else if versionOpt opts
      then putStrLn $ "ghccheck v" ++ showVersion version
    else if null (concat otherOpts ++ rest)
      then print argMode
    else do
      home <- getHomeDirectory
      let ghccheck = home </> ".ghccheck"
      gopts <- if noConfOpt opts
        then return []
        else fmap listGOpts $
               readGhci `catch` \(_ :: IOException) -> do
                 readGhccheck ghccheck `catch` \(_ :: IOException) -> do
                   putStrLn $ "Configuration file " ++ confFiles ++ " not found"
                   return ""
      let compiler = if runOpt opts then "runghc" else "ghc"
          modeOpts
            | interactiveOpt opts = ["--interactive", "-ignore-dot-ghci"]
            | runOpt opts         = ["-ignore-dot-ghci"]
            | otherwise           = ["--make", "-O0", "-no-link", "-dynamic"]
          allOpts = modeOpts
                 ++ ["-hidir .ghc-temp", "-odir .ghc-temp"]
                 ++ map Text.pack otherOpts
                 ++ gopts
          allOpts'
            | runOpt opts = map (\o -> "--ghc-arg=\"" <> o <> "\"") allOpts
            | otherwise   = allOpts

      let cmd = Text.unwords (compiler : allOpts' ++ [Text.pack rest])

      Text.putStrLn cmd
      stat <- system $ Text.unpack cmd
      case stat of
          ExitFailure _ -> fail $ show stat
          _ -> return ()
  where
    readGhci = do
        pwd  <- getCurrentDirectory
        putStrLn $ "Using configuration file " ++ pwd </> ".ghci"
        file <- readConfFile ".ghci"
        return file

    readGhccheck ghccheck = do
        putStrLn $ "Using configuration file " ++ ghccheck
        file <- readConfFile ghccheck
        return file

-- The `-no-link` flag should not be used in interactive mode. First, it causes
-- GHCi to expect a `main` method in the `Main` module, and second, it gives
-- strange results when reloading. Take the following file as an example:
--
--     module Module where
--
--     headd :: [a] -> a
--     headd []    = error "headd: empty list"
--     headd (a:_) = a
--
-- Load the file using
--
--     ghc --interactive -no-link Module.hs
--
-- Evaluate `headd`:
--
--     *Module> head []
--     *** Exception: Prelude.head: empty list
--
-- Comment out the line `headd [] = ...` and reload the file. Strangely `head`
-- behaves just like before the change:
--
--     *Module> head []
--     *** Exception: Prelude.head: empty list
--
-- This was tested on GHC 7.10.2.

-- The `-dynamic` flag is needed to make it possible for GHCi to use the
-- generated object files. It also seems to make compilation a bit faster.

