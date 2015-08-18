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

-- | A string representing git clone and github url.
-- github is prepended to all git repos in bundles.
github :: String
github = "git clone http://github.com/"

gitex :: String
gitex = ".git"

-- | Create a symbolic link for each file name listed in dotFiles.  Files
-- are symlinked from $HOME to $HOME/vim.
dotfiles :: [String]
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

-- | The bundle path relative to ~/git/dotfiles
bpath :: String
bpath = "vim/bundle"

-- | The vim bundles I am using.
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

-- | The colors I am using.
colors :: String -> String -> [String]
colors s x =
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
  setupDirectory bpath
  cloneRepos (bundles github gitex)

  -- Step 3: Clone github repos specifc to base16 colors.
  setupDirectory cpath
  cloneRepos (colors  github gitex)  

-- | makeSymoblicLink
makeSymbolicLink :: String -> IO ExitCode
makeSymbolicLink f = do
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ f
  system $ "rm -rf " ++ tfile
  
  c <- getCurrentDirectory
  let sfile = c ++ "/" ++ (takeFileName f)
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

-- | Safely remove the directory and all subfolders.
safelyRemoveDirectory :: FilePath -> IO ()
safelyRemoveDirectory fpath = do
  b <- doesDirectoryExist fpath
  case b of
    True ->  removeDirectoryRecursive fpath
    False -> return ()

-- | Clone repos I am interested in using.
cloneRepos :: [String] -> IO ()
cloneRepos name = do
  mapM_ system name 

