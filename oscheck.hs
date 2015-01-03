-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
module Main (main) where

import System.Directory
import System.Info

main = do
  putStrLn $ "Operating System: " ++ os
  putStrLn $ "    Architecture: " ++ arch
  putStrLn $ "   Compiler Name: " ++ compilerName
  home <- getHomeDirectory 
  putStrLn $ "  Home Directory: " ++ home

  case os of
    "darwin"  -> putStrLn "     My favorite: OSX"
    "linux"   -> putStrLn "   My 2nd choice: linux"
    "mingw32" -> putStrLn " Not my favorite: Windows"
    _         -> putStrLn "Operating system: Unknown"
