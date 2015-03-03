module Main where



import Control.Exception
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
    { helpOpt    :: Bool
    , versionOpt :: Bool
    , noConfOpt  :: Bool
    }
  deriving (Show)

defaultOptions = Options False False False

confFiles = "(.ghci or $HOME/.ghccheck)"

-- | Description of command line options
argMode :: Mode Options
argMode = mode
    "ghccheck"
    defaultOptions
    ("The ghccheck program v" ++ showVersion version)
    (flagArg (\_ _ -> Right defaultOptions) "...  (urecognized options and files passed on to GHC)")
    [ flagHelpSimple (\o -> o {helpOpt = True})
    , flagVersion (\o -> o {versionOpt = True})
    , flagNone
        ["n","no-conf"]
        (\o -> o {noConfOpt = True})
        ("Don't read a configuration file " ++ confFiles)
    ]

-- | All flags (including dashes) recognized by ghccheck
flags :: [String]
flags = map dash $ concatMap flagNames $ modeFlags argMode
  where
    dash f@[_] = '-':f
    dash f     = "--" ++ f

-- | Read the arguments (obtained by `getArgs`). The second result contains all unrecognized
-- arguments.
readArgs :: [String] -> (Options, [String])
readArgs args = (opts,rest)
  where
    rest = filter (`notElem` flags) args
    opts = Options
      { helpOpt    = "-?" `elem` args || "--help"    `elem` args
      , versionOpt = "-V" `elem` args || "--version" `elem` args
      , noConfOpt  = "-n" `elem` args || "--no-conf" `elem` args
      }

-- | Extract all options from a @.ghci@ file. Options are lines beginning with @:set @.
listGOpts :: Text -> [Text]
listGOpts = filter (/="") . map getOpt . Text.lines
  where
    getOpt l = case Text.splitAt 5 l of
        (":set ",opt) -> opt
        _ -> ""

-- Unfortunately, I wasn't able to use `processArgs` to parse the command line. The problem is that
-- I wasn't able to allow arbitrary additional options to be passed on to GHC. This is why I use
-- `fmap readArgs getArgs`.

main = do
    (opts,rest) <- fmap readArgs getArgs
    if helpOpt opts
      then print argMode
    else if versionOpt opts
      then putStrLn $ "ghccheck v" ++ showVersion version
    else if null rest
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
      let cmd = Text.unwords
            $  "ghc -O0 --make -no-link -hidir .ghc-temp -odir .ghc-temp"
            :  gopts
            ++ map Text.pack rest
      Text.putStrLn cmd
      stat <- system $ Text.unpack cmd
      case stat of
          ExitFailure _ -> error $ show stat
          _ -> return ()
  where
    readGhci = do
        pwd  <- getCurrentDirectory
        file <- Text.readFile ".ghci"
        putStrLn $ "Using configuration file " ++ pwd </> ".ghci"
        return file

    readGhccheck ghccheck = do
        file <- Text.readFile ghccheck
        putStrLn $ "Using configuration file " ++ ghccheck
        return file

