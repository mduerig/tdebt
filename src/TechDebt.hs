module TechDebt
  ( main,
  )
where

import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)


-- git log --pretty=format: --name-only
gitLog :: IO (ExitCode, String, String)
gitLog = readProcessWithExitCode "git" ["log", "--pretty=format:", "--name-only"] ""

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
