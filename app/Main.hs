module Main where

import System.Environment
import System.Exit
import qualified TechDebt

main :: IO ()
main = do
  args <- getArgs 
  runWithArgs args

runWithArgs :: [String] -> IO ()
runWithArgs ["-h"] = help >> exitSuccess
runWithArgs [path] = TechDebt.pmdHotspots path
runWithArgs ["pmd", path] = TechDebt.pmdHotspots path
runWithArgs ["loc", path] = TechDebt.locHotspots path
runWithArgs _ = help >> exitSuccess

help :: IO ()
help = putStrLn "Usage: [pmd|loc] [-h] <path to git repo>"

