-- | Copyright (c) Gary Allan Howard aka Traap.
-- Distributed under the same terms as Vim itself.  See :help license.
--
-- This program initializes my HOME directory with symbolic link references to
-- my git/dotfiles folders.
module Main (main) where

import Control.Monad
import System.Directory
import System.Exit
import System.FilePath
import System.Process

-- | A string representing git clone and Github URL.
-- Github is perpended to all git repos in bundles.
github :: String
github = "git clone http://github.com/"

-- | Create a symbolic link for each file name listed in dotfiles.  Files
-- are symlinked from $HOME to $HOME/vim.
dotfiles :: [String]
dotfiles =
  ["bash_profile"
  ,"bashrc"
  ,"emacs"
  ,"emacs.d"
  ,"gitconfig"
  ,"gitignore_global"
  ,"gvimrc"
  ,"inputrc"
  ,"ssh/config"
  ,"tmux.conf"
  ,"vim"
  ,"vimoutlinerrc"
  ,"vimrc"
  ,"vimrc_background"
  ]

-- | The autoload path relative to ~/git/dotfiles
apath :: String
apath = "vim/autoload"

-- | The vim bundles I am using.
bundles :: String -> [String]
bundles s = [s ++ "junegunn/vim-plug"]            -- Light weight plugin manager

-- | The colors I am using.
colors :: String -> [String]
colors s =
  [s ++ "chriskempson/base16-gnome-terminal"
  ,s ++ "chriskempson/base16-iterm2"
  ,s ++ "chriskempson/base16-shell"
  ]

-- | The color path relative to ~/git/dotfiles
cpath :: String
cpath = "color"

-- | Let's move some files around.
main :: IO ()
main = do
  -- Step 1: Setup symlinnks.
  mapM_ makeSymbolicLink dotfiles

  -- Step 2: Clone github repos specific to vim.
  setupDirectory apath
  cloneRepos $ bundles github

  -- Step 3: Clone github repos specifc to base16 colors.
  setupDirectory cpath
  cloneRepos $ colors  github

-- | makeSymoblicLink
makeSymbolicLink :: String -> IO ExitCode
makeSymbolicLink f = do
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ f
  _ <- system $ "rm -rf " ++ tfile

  c <- getCurrentDirectory
  let sfile = c ++ "/" ++ takeFileName f
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | Setup directory.
setupDirectory :: FilePath -> IO ()
setupDirectory fpath = do
  setDotFileDirectory
  safelyRemoveDirectory fpath
  createDirectoryIfMissing True fpath
  setCurrentDirectory fpath

-- | Set ~/dotfiles/vim directory
setDotFileDirectory :: IO ()
setDotFileDirectory = do
  h <- getHomeDirectory
  let f = h ++ "/git/dotfiles"
  setCurrentDirectory f

-- | Safely remove the directory and all sub-folders.
safelyRemoveDirectory :: FilePath -> IO ()
safelyRemoveDirectory fpath = do
  b <- doesDirectoryExist fpath
  Control.Monad.when b $ removeDirectoryRecursive fpath

-- | Clone repos I am interested in using.
cloneRepos :: [String] -> IO ()
cloneRepos = mapM_ system
