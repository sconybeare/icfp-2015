-- | Process command-line arguments
module CommandLine (Options (..), getOptions) where


--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import           Options.Applicative

import           Data.Foldable       (asum)
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text, pack)
import           System.FilePath     hiding ((<.>))

import           Utility             ((<.>))

-- HLint pragmas
{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


-- | Options specified by the ICFP 2015 problem specification
data Options =
  Options { optFiles       :: [FilePath] -- ^ Input files
          , optTimeLimit   :: Maybe Int  -- ^ # of seconds   available
          , optMemoryLimit :: Maybe Int  -- ^ # of MB of RAM available
          , optCores       :: Maybe Int  -- ^ # of cores     available
          , optPhrases     :: [Text]     -- ^ phrases of power
          } deriving (Eq, Show, Read)


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------


-- | This will take care of option parsing in your main function;
--   the type says it all, really.
getOptions :: IO Options
getOptions = execParser (info (helper <*> optParser) optInfo)
  where
    optInfo = fullDesc <> progDesc optDesc <> header optHeader



--------------------------------------------------------------------------------
---------------------------------- Constants -----------------------------------
--------------------------------------------------------------------------------


-- Strings

optDesc, optHeader :: String
optDesc   = "Solve the given ICFP 2015 JSON problem or problems."
optHeader = "play_icfp2015 - Solve the ICFP 2015 problem."


-- Parsers

fileOpt        :: Parser [FilePath]
timeLimitOpt   :: Parser (Maybe Int)
memoryLimitOpt :: Parser (Maybe Int)
coresOpt       :: Parser (Maybe Int)
phrasesOpt     :: Parser [Text]
optParser      :: Parser Options

fileOpt        = opt $ short     'f'
                    <> long      "file"
                    <> metavar   "JSON-FILE"
                    <> helpMulti "Input JSON file to parse."
  where
    opt = catMaybes <.> many . fileOption

timeLimitOpt   = opt $ short     't'
                    <> long      "time"
                    <> metavar   "NUM-SECONDS"
                    <> help      "Time limit in seconds."
  where
    opt = optional . option auto

memoryLimitOpt = opt $ short     'm'
                    <> long      "memory"
                    <> metavar   "NUM-MB"
                    <> help      "Memory limit in megabytes."
  where
    opt = optional . option auto

coresOpt       = opt $ short     'c'
                    <> long      "cores"
                    <> metavar   "NUM-CORES"
                    <> help      "Number of cores on the current machine."
  where
    opt = optional . option auto

phrasesOpt     = opt $ short     'p'
                    <> long      "phrase"
                    <> metavar   "PHRASE"
                    <> helpMulti "Phrase of power to specify"
  where
    opt = asum <.> optional . many . pack <.> strOption

optParser = Options
            <$> fileOpt
            <*> timeLimitOpt
            <*> memoryLimitOpt
            <*> coresOpt
            <*> phrasesOpt


--------------------------------------------------------------------------------
------------------------------ Utility functions -------------------------------
--------------------------------------------------------------------------------


fileOption :: Mod OptionFields String -> Parser (Maybe FilePath)
fileOption = validPath <.> strOption
  where
    validPath p = if isValid p then Just p else Nothing

multiMessage :: String
multiMessage = "This may be specified multiple times."

helpMulti :: String -> Mod f a
helpMulti msg = help (msg <> " " <> multiMessage)
