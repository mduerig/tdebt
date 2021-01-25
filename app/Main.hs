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
  { path ::  Maybe String
  , complexity :: Complexity
  , before :: Maybe String
  , after :: Maybe String
  , gitDir :: String
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
runWithOpts (Opts path PMD before after gitDir) = TechDebt.pmdHotspots gitDir before after path
runWithOpts (Opts path LOC before after gitDir) = TechDebt.locHotspots gitDir before after path

argsParser :: Parser Opts
argsParser = Opts
     <$>  optional (strArgument
          ( metavar "<path>"
         <> help "path within the Git repository"))
     <*>  ( flag PMD PMD (long "pmd"
         <> help "use pmd complexity metric")
        <|> flag PMD LOC (long "loc"
         <> help "use loc complexity metric"))
     <*> optional ( strOption
          ( metavar "<date>"
         <> long "before"
         <> short 'b'
         <> help "only include commits before the specified date" ))
     <*> optional ( strOption
          ( metavar "<date>"
         <> long "after"
         <> short 'a'
         <> help "only include commits after the specified date" ))
     <*> strOption
          ( metavar "<path>"
         <> long "git-dir"
         <> short 'g'
         <> help "path to the Git repository")