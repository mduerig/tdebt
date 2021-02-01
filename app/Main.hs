module Main where

import qualified TechDebt
import Options.Applicative
    ( Alternative((<|>)),
      optional,
      value,
      auto,
      option,
      flag,
      flag',
      help,
      info,
      long,
      metavar,
      short,
      strArgument,
      strOption,
      execParser,
      helper,
      internal,
      Parser )
import Data.Monoid ((<>))
import System.Directory (getCurrentDirectory)
import Paths_tdebt(getDataFileName)

data Opts = Opts
  { path ::  Maybe String
  , complexity :: Complexity
  , pmdRules :: String
  , gitDir :: String
  , after :: Maybe String
  , sum :: Maybe Int
  } deriving Show

data Complexity = PMD | LOC
  deriving Show

main :: IO ()
main = do
  cd <- getCurrentDirectory
  pmdRules <- getDataFileName "rule.xml"
  opts <- execParser (parserWithDefaultDir cd pmdRules)
  runWithOpts opts
  where
    parserWithDefaultDir cd ruleFile = info (helper <*> optsParser cd ruleFile) mempty

runWithOpts :: Opts -> IO ()
runWithOpts (Opts path PMD pmdRules gitDir after sum)
  = TechDebt.pmdHotspots gitDir after pmdRules sum path
runWithOpts (Opts path LOC _ gitDir after sum)
  = TechDebt.locHotspots gitDir after sum path

optsParser :: String -> String -> Parser Opts
optsParser dir pmdRules = Opts
     <$> optional path
     <*> ( pmd <|> loc )
     <*> rule
     <*> gitDir
     <*> optional after
     <*> optional summary
  where
    path = strArgument
        (  metavar "<path>"
        <> help "path within the Git repository"
        )
    pmd = flag PMD PMD
        (  long "pmd"
        <> help "use pmd complexity metric"
        )
    loc = flag PMD LOC
        (  long "loc"
        <> help "use loc complexity metric"
        )
    after = strOption
        (  metavar "<date>"
        <> long "after"
        <> short 'a'
        <> help "only include commits after the specified date"
        )
    rule = strOption
        (  metavar "<path>"
        <> long "rule"
        <> value pmdRules
        <> internal
        <> help "custom PMD rule.xml"
        )
    gitDir = strOption
        (  metavar "<path>"
        <> long "git-dir"
        <> short 'g'
        <> value dir
        <> help "path to the Git repository"
        )
    summary = option auto
        (  metavar "<count>"
        <> long "sum"
        <> short 's'
        <> help "output the sum of the last <count> metrics"
        )

