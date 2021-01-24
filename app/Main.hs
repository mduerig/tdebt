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
  { gitDir :: String
  , complexity :: Complexity
  , before :: Maybe String
  , after :: Maybe String
  , path :: Maybe String
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
runWithOpts (Opts gitDir PMD before after path) = TechDebt.pmdHotspots gitDir before after path
runWithOpts (Opts gitDir LOC before after path) = TechDebt.locHotspots gitDir before after path

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
     <*> optional ( strOption
          ( metavar "<path>"
         <> long "path"
         <> short 'p'
         <> help "Only include files from the given path within the Git repository" ))