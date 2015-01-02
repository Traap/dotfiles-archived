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
  safelyRemoveVimDirectory
  mapM_ Main.copyFile dotfiles
  copyVimFolder
  runVimGetBundles

-- | Safely remove .vim folder and all contents.
safelyRemoveVimDirectory :: IO ()
safelyRemoveVimDirectory = do 
  dir <- getHomeDirectory
  let vdir = dir ++ "/.vim"
  b <- doesDirectoryExist vdir
  case b of
    True  -> removeDirectoryRecursive vdir
    False -> return ()

-- | copy a file
-- ex vimrc ~/.vimrc
copyFile :: String -> IO ExitCode
copyFile file = do  
  dir <- getHomeDirectory
  let cmd = "cp -av " ++ file ++ " " ++ dir ++ "/." ++ file
  system cmd

-- | copy .vim folder 
copyVimFolder :: IO ExitCode 
copyVimFolder = do
  dir <- getHomeDirectory
  let cmd = "cp -av vim " ++ dir ++ "/.vim"
  system cmd

-- | run my getbundles.hs program
runVimGetBundles :: IO ExitCode
runVimGetBundles = do
  dir <- getHomeDirectory
  let vdir = dir ++ "/.vim"
  setCurrentDirectory vdir
  let cmd = "runhaskell getbundles.hs"
  system cmd
