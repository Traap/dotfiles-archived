-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initializes my HOME directory with symbolic link references to
-- my git/dotfiles folders.
module Main (main) where

import System.Directory
import System.Exit
import System.Process

-- | Create a symbolic link for the following files.
dotfiles = 
  ["bash_profile"
  ,"gitconfig"
  ,"inputrc"
  ,"vim"
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
  c <- getCurrentDirectory
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ f
  let sfile = c ++ "/" ++ f
  system $ "rm -rf " ++ tfile
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | run my getbundles.hs program
runVimGetBundles :: IO ExitCode
runVimGetBundles = do
  setCurrentDirectory "vim"
  system $ "runhaskell getbundles.hs"

