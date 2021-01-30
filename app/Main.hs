module Main where

import qualified TechDebt
import Options.Applicative
    ( Alternative((<|>)),
      optional,
      value,
      auto,
      option,
      flag,
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
  , before :: Maybe String
  , after :: Maybe String
  , pmdRules :: String
  , gitDir :: String
  , normChurn :: Maybe Double
  , normComplexity :: Maybe Double
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
runWithOpts (Opts path PMD before after pmdRules gitDir normChurn normComplexity)
  = TechDebt.pmdHotspots normChurn normComplexity gitDir pmdRules before after path
runWithOpts (Opts path LOC before after _ gitDir normChurn normComplexity)
  = TechDebt.locHotspots normChurn normComplexity gitDir before after path

optsParser :: String -> String -> Parser Opts
optsParser dir pmdRules = Opts
     <$> optional path
     <*> ( pmd <|> loc )
     <*> optional before
     <*> optional after
     <*> rule
     <*> gitDir
     <*> optional churnNorm
     <*> optional complexityNorm
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
    before = strOption
        (  metavar "<date>"
        <> long "before"
        <> short 'b'
        <> help "only include commits before the specified date"
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
        <> short 'r'
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
    churnNorm = option auto
        (  metavar "<churn norm>"
        <> long "churn-norm"
        <> help "constant for normalizing the churn value"
        )
    complexityNorm = option auto
        (  metavar "<complexity norm>"
        <> long "complexity-norm"
        <> help "constant for normalizing the complexity value"
        )