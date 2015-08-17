-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initializes my HOME directory with symbolic link references to
-- my git/dotfiles folders.
module Main (main) where

import System.Directory
import System.Exit
import System.FilePath
import System.Process

-- | Create a symbolic link for the following files.
dotfiles =
  ["bash_profile"
  ,"bashrc"
  ,"emacs"
  ,"emacs.d"
  ,"gitconfig"
  ,"gitignore_global"
  ,"inputrc"
  ,"ssh/config"
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
  let sfile = c ++ "/" ++ (takeFileName f)
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | run my getbundles.hs program
runVimGetBundles :: IO ExitCode
runVimGetBundles = do
  setCurrentDirectory "vim"
  system $ "runhaskell getbundles.hs"

