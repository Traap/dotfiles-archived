-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
module Main (main) where

import Data.List
import System.Directory
import System.Environment
import System.Info

osx = "         OS Name: OSX"
lin = "         OS Name: Linux"
win = "         OS Name: Windows"
unk = "         OS Name: "

main = do
  putStrLn "\nSystem Environment"
  putStrLn $ "Operating System: " ++ os
  case os of
    "darwin"  -> putStrLn osx
    "linux"   -> putStrLn lin
    "mingw32" -> putStrLn win
    _         -> putStrLn $ unk ++ os
  putStrLn $ "    Architecture: " ++ arch
  putStrLn $ "   Compiler Name: " ++ compilerName
  home <- getHomeDirectory 
  putStrLn $ "  Home Directory: " ++ home


  putStrLn "\nEnvironment Variables:"
  env <- getEnvironment >>= (return.sort)
  mapM_ printEnv env

printEnv :: (String, String) -> IO ()
printEnv (key, value) = do
  putStrLn $ key ++ ": " ++ value
