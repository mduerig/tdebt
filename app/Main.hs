{-#LANGUAGE OverloadedStrings #-}

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
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy.Char8 as BSL

data Opts = Opts
  { path ::  Maybe String
  , complexity :: Complexity
  , perFile :: Bool
  , pmdRules :: String
  , gitDir :: String
  , after :: Maybe String
  , sum :: Maybe Int
  , outputFormat :: OutputFormat
  } deriving Show

data Complexity = PMD | LOC
  deriving Show

data OutputFormat = RAW | CSV
  deriving Show

main :: IO ()
main = do
  cd <- getCurrentDirectory
  pmdRules <- getDataFileName "rule.xml"
  opts <- execParser (parserWithDefaultDir cd pmdRules)
  runWithOpts opts
  where
    parserWithDefaultDir cd ruleFile = info (helper <*> optsParser cd ruleFile) mempty

csvEncode :: TechDebt.TDebt -> BSL.ByteString
csvEncode tDebt =
  let
    tabulate (file, TechDebt.Metric churn complexity debt) = (file, churn, complexity, debt)
  in
    CSV.encode $ tabulate <$> tDebt

raw :: TechDebt.TDebt -> IO ()
raw = mapM_ print

csv :: TechDebt.TDebt -> IO ()
csv tDebt = BSL.putStr $ csvEncode tDebt

toFormat :: OutputFormat -> TechDebt.TDebt -> IO ()
toFormat RAW = raw
toFormat CSV = csv

runWithOpts :: Opts -> IO ()
runWithOpts (Opts path complexity perFile pmdRules gitDir after count format) = do
  tDebt <- case complexity of
    PMD -> TechDebt.pmdHotspots gitDir after pmdRules path
    LOC -> TechDebt.locHotspots gitDir after path
  case tDebt of
    Left msg -> putStrLn msg
    Right debt ->
        toFormat format
      $ TechDebt.maybeSum count
      $ TechDebt.maybeNorm perFile
        debt

optsParser :: String -> String -> Parser Opts
optsParser dir pmdRules = Opts
     <$> optional path
     <*> ( pmd <|> loc )
     <*> perFile
     <*> rule
     <*> gitDir
     <*> optional after
     <*> optional summary
     <*> (raw <|> csv)
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
    perFile = flag False True
        (  long "per-file"
        <> help "Normalize the debt value by the total number of files"
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
    raw = flag RAW RAW
        (  long "raw"
        <> help "raw output format"
        )
    csv = flag RAW CSV
        (  long "csv"
        <> help "CSV output format"
        )

