module Main where

import qualified TechDebt
import Options.Applicative
    ( Alternative((<|>)),
      optional,
      value,
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
import System.Directory (getCurrentDirectory)

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
  cd <- getCurrentDirectory
  opts <- execParser (parserWithDefaultDir cd)
  runWithOpts opts
  where
    parserWithDefaultDir cd = info (helper <*> argsParser cd) mempty

runWithOpts :: Opts -> IO ()
runWithOpts (Opts path PMD before after gitDir) = TechDebt.pmdHotspots gitDir before after path
runWithOpts (Opts path LOC before after gitDir) = TechDebt.locHotspots gitDir before after path

argsParser :: String -> Parser Opts
argsParser dir = Opts
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
         <> value dir
         <> help "path to the Git repository")