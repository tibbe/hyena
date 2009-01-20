------------------------------------------------------------------------
-- |
-- Module      :  Hyena.Config
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module specifies the server configuration.
--
------------------------------------------------------------------------

module Hyena.Config
    ( Config(..),
      configFromFlags,
      defaultConfig
    ) where

import Control.Monad (when)
import Data.Monoid (Monoid(..))
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>), dropFileName)
import System.IO (BufferMode(..), Handle, IOMode(..), hSetBuffering, openFile,
                  stderr)

-- ---------------------------------------------------------------------
-- Config type

-- | The server configuration.
data Config = Config
    { address   :: String
    -- ^ Address (hostname or IP) to bind to when listening for
    -- connections.
    , daemonize :: Bool
    -- ^ Run in the background.
    , debug     :: Bool
    -- ^ Print lots of debug information.
    , logHandle :: Handle
    -- ^ Where to dump log messages in daemon mode.
    , port      :: Int
    -- ^ Port to bind to when listening for connections.
    } deriving Show

-- | Converts a set of flags into a server configuration.
flagsToConfig :: Flags -> IO Config
flagsToConfig flags = do
  when (flag flagDaemonize) $
       createDirectoryIfMissing True $ dropFileName (flag flagLogFile)
  logHandle' <- if flag flagDaemonize
                then openFile (flag flagLogFile) AppendMode
                else return stderr
  hSetBuffering logHandle' LineBuffering
  return Config
             { address   = flag flagAddress
             , daemonize = flag flagDaemonize
             , debug     = flag flagDebug
             , logHandle = logHandle'
             , port      = flag flagPort
             }
    where flag field = fromFlag $ field flags

-- | Reads the server options from the command line. Settings from
-- 'defaultConfig' is used for unspecified options. Creates missing
-- directories as needed for the log file referred to by the @--log@
-- flag when in 'daemonize'd mode.
configFromFlags :: IO Config
configFromFlags = do
  argv <- getArgs
  cwd <- getCurrentDirectory
  progName <- getProgName
  case parseArgs argv progName of
    Left err    -> putStr err >> exitFailure
    Right flags -> flagsToConfig $ defaultFlags cwd `mappend` flags

-- | A set of default options most users should use. Creates missing
-- directories as needed for the default log file when in 'daemonize'd
-- mode.
defaultConfig :: IO Config
defaultConfig = do
  cwd <- getCurrentDirectory
  flagsToConfig $ defaultFlags cwd

-- ---------------------------------------------------------------------
-- Flag type

data Flag a = Flag a | NoFlag deriving Show

instance Functor Flag where
    fmap f (Flag x) = Flag (f x)
    fmap _ NoFlag   = NoFlag

instance Monoid (Flag a) where
    mempty = NoFlag
    _ `mappend` f@(Flag _) = f
    f `mappend` NoFlag     = f

fromFlag :: Flag a -> a
fromFlag (Flag x) = x
fromFlag NoFlag   = error "fromFlag NoFlag"

-- ---------------------------------------------------------------------
-- Config flags

data Flags = Flags
    { flagAddress   :: Flag String
    , flagDaemonize :: Flag Bool
    , flagDebug     :: Flag Bool
    , flagLogFile   :: Flag FilePath
    , flagPort      :: Flag Int
    } deriving Show

defaultFlags :: FilePath -> Flags
defaultFlags cwd =
    -- NOTE: If we add a flag to change the working directory it has
    -- to be taken into account here.
    Flags { flagAddress   = Flag "0.0.0.0"
          , flagDaemonize = Flag False
          , flagDebug     = Flag False
          , flagLogFile   = Flag $ cwd </> "log/hyena.log"
          , flagPort      = Flag 3000
          }

emptyFlags :: Flags
emptyFlags = mempty

instance Monoid Flags where
    mempty = Flags
             { flagAddress   = mempty
             , flagDaemonize = mempty
             , flagDebug     = mempty
             , flagLogFile   = mempty
             , flagPort      = mempty
             }
    mappend a b = Flags
                  { flagAddress   = combine flagAddress
                  , flagDaemonize = combine flagDaemonize
                  , flagDebug     = combine flagDebug
                  , flagLogFile   = combine flagLogFile
                  , flagPort      = combine flagPort
                  }
        where combine field = field a `mappend` field b

-- ---------------------------------------------------------------------
-- Args parsing

-- | Converts a 'String' containing a port number to an integer and
-- fails with an 'error' if the 'String' contained non-digit
-- characters.
flagToPort :: String -> Int
flagToPort str =
    case reads str of
      [(i, "")] -> i
      _         -> error $ "--port: invalid port `" ++ str ++ "'"

-- | The command line options.
options :: [OptDescr (Flags -> Flags)]
options =
    [Option "a" ["address"]
                (reqArgFlag "ADDRESS" flagAddress
                                (\v flags -> flags {flagAddress = v}))
                "bind to ADDRESS (hostname or IP) on localhost"
    ,Option "d" ["daemonize"]
                (trueArg flagDaemonize (\v flags -> flags {flagDaemonize = v}))
                "run in the background"
    ,Option "B" ["debug"]
                (trueArg flagDebug (\v flags -> flags {flagDebug = v}))
                "print lots of debug information"
    ,Option "l" ["log"]
                (reqArgFlag "FILE" flagLogFile
                                (\v flags -> flags {flagLogFile = v}))
                "dump log messages to FILE when daemonized"
    ,Option "p" ["port"]
                (reqArg "PORT" (Flag . flagToPort)
                        flagPort (\v flags -> flags {flagPort = v}))
                "bind to PORT on localhost"
    ]

-- | Parses the given command line arguments.  Returns either the
-- parsed flags or a 'String' explaining the error on failure.
parseArgs :: [String] -> String -> Either String Flags
parseArgs argv progName =
    case getOpt Permute options argv of
      (flags, _, []) -> Right $ foldl (flip id) emptyFlags flags
      (_, _, errs)   -> Left $ concat errs ++ usageInfo header options
          where header = "Usage: " ++ progName ++ " [OPTION]..."

-- ---------------------------------------------------------------------
-- GetOpt helpers

reqArg :: (Monoid a) =>
          String -> (String -> a) -> (t -> a) -> (a -> t -> t1)
                 -> ArgDescr (t -> t1)
reqArg name mkFlag get set =
    ReqArg (\v flags -> set (get flags `mappend` mkFlag v) flags) name

noArg :: (Monoid a) => a -> (t -> a) -> (a -> t -> t1) -> ArgDescr (t -> t1)
noArg flag get set =
    NoArg (\flags -> set (get flags `mappend` flag) flags)

trueArg :: (t -> Flag Bool) -> (Flag Bool -> t -> t1)
        -> ArgDescr (t -> t1)
trueArg  = noArg (Flag True)

reqArgFlag :: String -> (t -> Flag String) -> (Flag String -> t -> t1)
              -> ArgDescr (t -> t1)
reqArgFlag name = reqArg name Flag

