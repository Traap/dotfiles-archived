-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initializes my OSX environment from my git/dotfiles
-- folder.
module Main (main) where

import System.Directory
import System.Exit
import System.Process

-- | Thie files I want copied to my home folder.
dotfiles = 
  ["bash_profile"
  ,"gitconfig"
  ,"inputrc"
  ,"vimrc"
  ]

-- | Let's move some files around.
main :: IO ExitCode 
main = do 
  mapM_ Main.copyFile dotfiles
  runVimGetBundles

-- | copy a file
-- ex vimrc ~/.vimrc
copyFile :: String -> IO ExitCode
copyFile file = do  
  dir <- getHomeDirectory
  let cmd = "cp -av " ++ file ++ " " ++ dir ++ "/." ++ file
  system cmd

-- | run my getbundles.hs program
runVimGetBundles :: IO ExitCode
runVimGetBundles = do
  dir <- getCurrentDirectory
  let vdir = dir ++ "/vim"
  setCurrentDirectory vdir
  let cmd = "runhaskell getbundles.hs"
  system cmd
