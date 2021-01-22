module Main where

import qualified TechDebt
import Options.Applicative
    ( Alternative((<|>)),
      optional,
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
      Parser )
import Data.Monoid ((<>))

data Opts = Opts
  { path :: String
  , complexity :: Complexity
  , before :: Maybe String
  , after :: Maybe String
  } deriving Show

data Complexity = PMD | LOC
  deriving Show

main :: IO ()
main = do
  opts <- execParser args
  runWithOpts opts
  where
    args = info (helper <*> argsParser) mempty

runWithOpts :: Opts -> IO ()
runWithOpts (Opts path PMD before after) = TechDebt.pmdHotspots path before after
runWithOpts (Opts path LOC before after) = TechDebt.locHotspots path before after

argsParser :: Parser Opts
argsParser = Opts
     <$> strArgument
          ( metavar "<path to git repository>"
         <> help "Path to the .git folder of a Git repository" )
     <*>  ( flag PMD PMD (long "pmd"
         <> help "use pmd complexity metric")
        <|> flag PMD LOC (long "loc"
         <> help "use loc complexity metric"))
     <*> optional ( strOption
          ( metavar "<date>"
         <> long "before"
         <> short 'b'
         <> help "Only include commits before the specified date" ))
     <*> optional ( strOption
          ( metavar "<date>"
         <> long "after"
         <> short 'a'
         <> help "Only include commits after the specified date" ))