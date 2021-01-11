module Main where

import System.Environment
import System.Exit
import qualified TechDebt

main :: IO ()
main = getArgs >>= parse

parse :: [[Char]] -> IO ()
parse ["-h"] = help >> exitSuccess
parse [path] = TechDebt.pmdHotspots path
parse ["pmd", path] = TechDebt.pmdHotspots path
parse ["loc", path] = TechDebt.locHotspots path
parse _ = help >> exitSuccess

help :: IO ()
help = putStrLn "Usage: [pmd|loc] [-h] <path to git repo>"

