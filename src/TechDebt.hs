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

(++?) :: [a] -> Maybe a -> [a]
(++?) xs Nothing = xs
(++?) xs (Just x) = xs ++ [x]

gitLog :: String -> Maybe String -> Maybe String -> IO (ExitCode, String, String)
gitLog gitDir after path =
  let
    afterDate = fmap ("--after=" ++) after
    args = ["--git-dir", ensureSlash gitDir ++ ".git", "log", "--pretty=format:", "--name-only"]
  in
    readProcessWithExitCode "git" (args ++? afterDate ++ ["--"] ++? path) ""

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

instance Semigroup Metric where
  (<>) (Metric x1 y1 z1) (Metric x2 y2 z2) = Metric (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Metric where
  mempty = Metric 0 0 0

type TDebt = [(String, Metric)]

techDebt :: Map String Int -> Map String Int -> TDebt
techDebt churn complexity =
  let
    mul (Metric churn _ _) (Metric _ complexity _) = Metric churn complexity (churn * complexity)
    nonZeroDebt (_, Metric _ _ debt) = debt /= 0
  in
    sortBy (compare `on` debt . snd)
    $ filter nonZeroDebt
    $ toList
    $ unionWith mul
      ( (\x -> Metric (fromIntegral x) 0 0.0) <$> churn )
      ( (\x -> Metric 0 (fromIntegral x) 0.0) <$> complexity )

maybeNorm :: Bool -> TDebt -> TDebt
maybeNorm perFile metrics =
  let
    norm (file, Metric churn complexity debt)
       = (file, Metric churn complexity (debt / fromIntegral (length metrics)))
  in
    if perFile then norm <$> metrics
    else metrics

maybeSum :: Maybe Int -> TDebt -> TDebt
maybeSum Nothing metrics = metrics
maybeSum (Just count) metrics =
  let
    sumMetric count metrics = mconcat $ take count $ reverse metrics
  in
    [("*", sumMetric count (snd <$> metrics))]

pmdHotspots :: String -> Maybe String -> String ->  Maybe String -> IO (Either String TDebt)
pmdHotspots gitDir after pmdRules path = do
  (gitExitCode, gitOut, gitErr) <- gitLog gitDir after path
  if gitExitCode /= ExitSuccess
    then return
      $ Left
      $ "Error while running git: " ++ gitErr
    else do
      (pmdExitCode, pmdOut, pmdErr) <- pmd gitDir pmdRules
      if pmdExitCode `notElem` [ExitSuccess, ExitFailure 4]
        then return
          $ Left
          $ "Error while running PMD: " ++ pmdErr
        else return
          $ Right
          $ techDebt (frequencies gitOut) (pmdComplexities gitDir pmdOut)

locHotspots :: String -> Maybe String ->  Maybe String ->  IO (Either String TDebt)
locHotspots gitDir after path = do
  (gitExitCode, gitOut, gitErr) <- gitLog gitDir after path
  if gitExitCode /= ExitSuccess
    then return
      $ Left
      $ "Error while running git: " ++ gitErr
    else do
      (_, locOut, _) <- loc gitDir
      return
        $ Right
        $ techDebt (frequencies gitOut) (locComplexities gitDir locOut)
