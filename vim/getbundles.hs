-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initialized my Vim environment with bundles I found useful
-- while I was learning Vim.  As a side note, I decided to learn Vim when
-- I started programming in Haskell.
module Main (main) where

import System.Directory
import System.Exit
import System.Process

-- | The bundles I am using.
bundles =
  ["git clone https://github.com/bling/vim-airline.git"
  ,"git clone https://github.com/christoomey/vim-tmux-navigator.git"
  ,"git clone https://github.com/moll/vim-bbye.git"
  ,"git clone https://github.com/raichoo/haskell-vim"
  ,"git clone https://github.com/scrooloose/nerdtree.git"
  ,"git clone https://github.com/scrooloose/syntastic.git"
  ,"git clone https://github.com/tpope/vim-commentary.git"
  ,"git clone https://github.com/tpope/vim-fugitive.git"
  ,"git clone https://github.com/tpope/vim-pathogen.git"
  ]

-- | The bundle path relative to ~/.vim
fpath = "bundle"

-- | Setup the directory and clone the Vim bundles.
main :: IO ()
main = do
  setupBundleDirectory fpath
  cloneBundles bundles

-- | Setup the bundle directory.
setupBundleDirectory :: FilePath -> IO ()
setupBundleDirectory fpath = do
  safelyRemoveBundleDirectory fpath
  createDirectoryIfMissing True fpath
  setCurrentDirectory fpath

-- | Safely remove the bundle directory and all subfolders.
safelyRemoveBundleDirectory :: FilePath -> IO ()
safelyRemoveBundleDirectory fpath = do
  b <- doesDirectoryExist fpath
  case b of
    True ->  removeDirectoryRecursive fpath
    False -> return ()

-- | Clone Vim bundles I am interested in using.
cloneBundles :: [String] -> IO ()
cloneBundles bundle = do
  mapM_ system bundle
