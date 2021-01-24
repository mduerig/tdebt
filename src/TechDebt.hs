module TechDebt where

import System.Exit ( ExitCode(..) )
import System.Process ( readProcessWithExitCode )
import Data.Map.Strict (fromListWith, toList, Map, unionWith, elems)
import Data.Function ( on )
import Data.List.Extra ( sortBy, split )
import Data.Bifunctor ( Bifunctor(bimap) )
import Text.Regex.Posix ( (=~) )

ensureSlash :: String -> String
ensureSlash [] = "/"
ensureSlash s = if last s == '/' then s else s ++ "/"

maybeAppend :: [a] -> Maybe a -> [a]
maybeAppend xs Nothing = xs
maybeAppend xs (Just x) = xs ++ [x]

gitLog :: String -> Maybe String -> Maybe String -> IO (ExitCode, String, String)
gitLog path before after =
  let
    beforeDate = fmap ("--before=" ++) before
    afterDate = fmap ("--after=" ++) after
    args = ["--git-dir", ensureSlash path ++ ".git", "log", "--pretty=format:", "--name-only"]
  in
    readProcessWithExitCode "git" (maybeAppend (maybeAppend args beforeDate) afterDate) ""

pmd :: String -> IO(ExitCode, String, String)
pmd path = readProcessWithExitCode "pmd"
  ["pmd", "-d", path, "-R", "rule.xml"] ""

loc :: String -> IO(ExitCode, String, String)
loc path = readProcessWithExitCode "bash"
  ["-c", "git --git-dir " ++ ensureSlash path ++ ".git  ls-files | xargs printf -- " ++ ensureSlash path ++ "'%s\n' | xargs wc -l"] ""

frequencies :: String -> Map String Int
frequencies =
  let
    occurrences xs = fromListWith (+) [(x, 1) | x <- xs]
    nonEmpty = filter ("" /=)
  in
    occurrences . nonEmpty . lines

parseFileName :: String -> String -> String
parseFileName path s =
  let
    fileName :: (String, String, String, [String]) -> String
    fileName (_, _, _, name:nil) = name
    fileName _ = "Could not parse file name from:" ++ s
  in
    fileName (s =~ (ensureSlash path ++ "([^:]*):.*"))

parsePmdComplexity :: String -> Int
parsePmdComplexity s =
  let
    complexity :: (String, String, String, [String]) -> Int
    complexity (_, _, _, c:nil) = read c
    complexity _ = 0
  in
    complexity (s =~ ".*total cyclomatic complexity of ([0-9]+).*")

pmdComplexities :: String -> String -> Map String Int
pmdComplexities path pmdOut =
  let
    splitOnTabs = split ('\t' ==)
    firstAndThird xs = (head xs, xs !! 2)
    tuples = firstAndThird . splitOnTabs <$> lines pmdOut
    parsed = bimap (parseFileName path) parsePmdComplexity
           . firstAndThird . splitOnTabs <$> lines pmdOut
  in
    fromListWith (+) parsed

parseLoc :: String -> String -> (String, Int)
parseLoc path line =
  let
    parse :: (String, String, String, [String]) -> (String, Int)
    parse (_, _, _, [complexity, fileName]) = (fileName, read complexity)
    parse _ = ("Could not parse output from wc -l:" ++ line, 0)
  in
    parse (line =~ ("[^0-9]*([0-9]+)[^/]*" ++ ensureSlash path ++ "(.*).*"))

locComplexities :: String -> String -> Map String Int
locComplexities path locOut =
  let
    valued = parseLoc path <$> lines locOut
  in
    fromListWith (+) valued

data Metric = Metric
  { churn :: Int
  , complexity :: Int
  , debt :: Double
  }
  deriving Show

techDebt :: Map String Int -> Map String Int -> [(String, Metric)]
techDebt churn complexity =
  let
    maxChurn      = fromIntegral $ maximum $ elems churn
    maxComplexity = fromIntegral $ maximum $ elems complexity
    mul (Metric churn _ _) (Metric _ complexity _)
      = Metric churn complexity ((fromIntegral churn / maxChurn) * (fromIntegral complexity / maxComplexity))
    nonZeroDebt (_, Metric _ _ debt) = debt /= 0
  in
    sortBy (compare `on` debt . snd)
    $ filter nonZeroDebt
    $ toList
    $ unionWith mul
      ( (\x -> Metric x 0 0.0) <$> churn )
      ( (\x -> Metric 0 x 0.0) <$> complexity )

pmdHotspots :: String -> Maybe String -> Maybe String -> IO ()
pmdHotspots path before after = do
  (gitExitCode, gitOut, gitErr) <- gitLog path before after
  if gitExitCode /= ExitSuccess
    then do
      putStrLn "Error while running git: "
      putStrLn gitErr
    else do
      (pmdExitCode, pmdOut, pmdErr) <- pmd path
      if pmdExitCode `notElem` [ExitSuccess, ExitFailure 4]
        then do
          putStrLn "Error while running PMD:"
          putStrLn pmdErr
        else do
          mapM_ print
          $ techDebt (frequencies gitOut) (pmdComplexities path pmdOut)

locHotspots :: String -> Maybe String -> Maybe String -> IO ()
locHotspots path before after = do
  (gitExitCode, gitOut, gitErr) <- gitLog path before after
  if gitExitCode /= ExitSuccess
    then do
      putStrLn "Error while running git: "
      putStrLn gitErr
    else do
      (_, locOut, _) <- loc path
      mapM_ print
        $ techDebt (frequencies gitOut) (locComplexities path locOut)
