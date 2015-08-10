-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initializes my HOME directory with symbolic link references to
-- my git/dotfiles folders.
module Main (main) where

import Data.List.Split
import System.Directory
import System.Exit
import System.Process

-- | Return the last word of a string.
lastWord :: String -> String
lastWord = last . splitOn "/"

-- | Create a symbolic link for the following files.
dotfiles =
  ["ssh/config"
  ,"bash_profile"
  ,"bashrc"
  ,"gitconfig"
  ,"gitignore_global"
  ,"inputrc"
  ,"tmux.conf"
  ,"vim"
  ,"vimoutlinerrc"
  ,"vimrc"
  ]

-- | Let's move some files around.
main :: IO ExitCode
main = do
  mapM_ makeSymbolicLink dotfiles
  runVimGetBundles

-- | makeSymoblicLink
makeSymbolicLink :: String -> IO ExitCode
makeSymbolicLink f = do
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ f
  system $ "rm -rf " ++ tfile
  
  c <- getCurrentDirectory
  let sfile = c ++ "/" ++ (lastWord f)
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | run my getbundles.hs program
runVimGetBundles :: IO ExitCode
runVimGetBundles = do
  setCurrentDirectory "vim"
  system $ "runhaskell getbundles.hs"

