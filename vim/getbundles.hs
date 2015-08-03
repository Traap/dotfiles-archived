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

-- | A string representing git clone and github url.
-- github is prepended to all git repos in bundles.
github = "git clone http://github.com/"
gitex = ".git"

-- | The bundles I am using.
bundles :: String -> String -> [String]
bundles s x =
  [s ++ "bling/vim-airline"
  ,s ++ "chriskempson/base16-vim"
  ,s ++ "christoomey/vim-tmux-navigator"
  ,s ++ "edkolev/tmuxline.vim"
  ,s ++ "moll/vim-bbye"
  ,s ++ "neovimhaskell/haskell-vim"
  ,s ++ "scrooloose/nerdtree"
  ,s ++ "scrooloose/syntastic"
  ,s ++ "tpope/vim-commentary"
  ,s ++ "tpope/vim-dispatch"
  ,s ++ "tpope/vim-fugitive"
  ,s ++ "tpope/vim-pathogen"
  ,s ++ "vim-scripts/bufexplorer.zip"
  ,s ++ "vimoutliner/vimoutliner"
  ]

-- | The bundle path relative to ~/.vim
fpath = "bundle"

-- | Setup the directory and clone the Vim bundles.
main :: IO ()
main = do
  setupBundleDirectory fpath
  cloneBundles (bundles github gitex)

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
