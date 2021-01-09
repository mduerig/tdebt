module TechDebt
  ( main,
  )
where

import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)


gitLog :: IO (ExitCode, String, String)
gitLog = readProcessWithExitCode "ls" [".", "--almost-all", "-l", "-S"] ""

main :: IO ()
main = do
  (exitCode, stdOut, stdErr) <- gitLog
  putStrLn $
    "Exit code: "
      ++ show exitCode
      ++ "\nOut: "
      ++ stdOut
      ++ "\nErr: "
      ++ stdErr
