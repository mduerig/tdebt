module TechDebt where

import System.Exit ( ExitCode(..) )
import System.Process ( readProcessWithExitCode )
import Data.Map.Strict (fromListWith, toList, Map, unionWith, elems)
import Data.Function ( on )
import Data.List.Extra ( sortBy, split )
import Data.Bifunctor ( Bifunctor(bimap) )
import Text.Regex.Posix ( (=~) )
import Data.Maybe

ensureSlash :: String -> String
ensureSlash [] = "/"
ensureSlash s = if last s == '/' then s else s ++ "/"

(++?) :: [a] -> Maybe a -> [a]
(++?) xs Nothing = xs
(++?) xs (Just x) = xs ++ [x]

gitLog :: String -> Maybe String -> Maybe String -> Maybe String -> IO (ExitCode, String, String)
gitLog gitDir before after path =
  let
    beforeDate = fmap ("--before=" ++) before
    afterDate = fmap ("--after=" ++) after
    args = ["--git-dir", ensureSlash gitDir ++ ".git", "log", "--pretty=format:", "--name-only"]
  in
    readProcessWithExitCode "git" (args ++? beforeDate ++? afterDate ++ ["--"] ++? path) ""

pmd :: String -> String -> IO(ExitCode, String, String)
pmd gitDir pmdRules = readProcessWithExitCode "pmd"
  ["pmd", "-d", gitDir, "-R", pmdRules] ""

loc :: String -> IO(ExitCode, String, String)
loc gitDir = readProcessWithExitCode "bash"
  ["-c", "git --git-dir " ++ ensureSlash gitDir ++ ".git  ls-files | xargs printf -- " ++ ensureSlash gitDir ++ "'%s\n' | xargs wc -l"] ""

frequencies :: String -> Map String Int
frequencies =
  let
    occurrences xs = fromListWith (+) [(x, 1) | x <- xs]
    nonEmpty = filter ("" /=)
  in
    occurrences . nonEmpty . lines

parseFileName :: String -> String -> String
parseFileName gitDir s =
  let
    fileName :: (String, String, String, [String]) -> String
    fileName (_, _, _, name:nil) = name
    fileName _ = "Could not parse file name from:" ++ s
  in
    fileName (s =~ (ensureSlash gitDir ++ "([^:]*):.*"))

parsePmdComplexity :: String -> Int
parsePmdComplexity s =
  let
    complexity :: (String, String, String, [String]) -> Int
    complexity (_, _, _, c:nil) = read c
    complexity _ = 0
  in
    complexity (s =~ ".*total cyclomatic complexity of ([0-9]+).*")

pmdComplexities :: String -> String -> Map String Int
pmdComplexities gitDir pmdOut =
  let
    splitOnTabs = split ('\t' ==)
    firstAndThird xs = (head xs, xs !! 2)
    tuples = firstAndThird . splitOnTabs <$> lines pmdOut
    parsed = bimap (parseFileName gitDir) parsePmdComplexity
           . firstAndThird . splitOnTabs <$> lines pmdOut
  in
    fromListWith (+) parsed

parseLoc :: String -> String -> (String, Int)
parseLoc gitDir line =
  let
    parse :: (String, String, String, [String]) -> (String, Int)
    parse (_, _, _, [complexity, fileName]) = (fileName, read complexity)
    parse _ = ("Could not parse output from wc -l:" ++ line, 0)
  in
    parse (line =~ ("[^0-9]*([0-9]+)[^/]*" ++ ensureSlash gitDir ++ "(.*).*"))

locComplexities :: String -> String -> Map String Int
locComplexities gitDir locOut =
  let
    valued = parseLoc gitDir <$> lines locOut
  in
    fromListWith (+) valued

data Metric = Metric
  { churn :: Double
  , complexity :: Double
  , debt :: Double
  }
  deriving Show

techDebt :: Maybe Double -> Maybe Double -> Map String Int -> Map String Int -> [(String, Metric)]
techDebt normChurn normComplexity  churn complexity =
  let
    maxChurn      = fromIntegral $ maximum $ elems churn
    maxComplexity = fromIntegral $ maximum $ elems complexity
    mul (Metric churn _ _) (Metric _ complexity _) = Metric churn complexity (churn * complexity)
    nonZeroDebt (_, Metric _ _ debt) = debt /= 0
  in
    sortBy (compare `on` debt . snd)
    $ filter nonZeroDebt
    $ toList
    $ unionWith mul
      ( (\x -> Metric (fromIntegral x / fromMaybe maxChurn normChurn) 0 0.0) <$> churn )
      ( (\x -> Metric 0 (fromIntegral x / fromMaybe maxComplexity normComplexity) 0.0) <$> complexity )

pmdHotspots :: Maybe Double -> Maybe Double -> String -> String -> Maybe String -> Maybe String -> Maybe String -> IO ()
pmdHotspots normChurn normComplexity gitDir pmdRules before after path = do
  (gitExitCode, gitOut, gitErr) <- gitLog gitDir before after path
  if gitExitCode /= ExitSuccess
    then do
      putStrLn "Error while running git: "
      putStrLn gitErr
    else do
      (pmdExitCode, pmdOut, pmdErr) <- pmd gitDir pmdRules
      if pmdExitCode `notElem` [ExitSuccess, ExitFailure 4]
        then do
          putStrLn "Error while running PMD:"
          putStrLn pmdErr
        else do
          mapM_ print
          $ techDebt normChurn normComplexity (frequencies gitOut) (pmdComplexities gitDir pmdOut)

locHotspots :: Maybe Double -> Maybe Double -> String -> Maybe String -> Maybe String -> Maybe String ->  IO ()
locHotspots normChurn normComplexity gitDir before after path = do
  (gitExitCode, gitOut, gitErr) <- gitLog gitDir before after path
  if gitExitCode /= ExitSuccess
    then do
      putStrLn "Error while running git: "
      putStrLn gitErr
    else do
      (_, locOut, _) <- loc gitDir
      mapM_ print
        $ techDebt normChurn normComplexity (frequencies gitOut) (locComplexities gitDir locOut)
