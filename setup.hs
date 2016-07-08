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
bundles :: String -> [String]
bundles s =
  [s ++ "bling/vim-airline"                      -- Status/tabline 
  ,s ++ "bruno-/vim-man"                         -- View man pages in vim
  ,s ++ "chriskempson/base16-vim"                -- iTerm2 terminal themes
  ,s ++ "christoomey/vim-tmux-navigator"         -- Navigate Tmux.
  ,s ++ "christoomey/vim-tmux-runner"            -- Vim and Tmux integration
  ,s ++ "ctrlpvim/ctrlp.vim"                     -- Fuzzy file find
  ,s ++ "edkolev/tmuxline.vim"                   -- Tmux status line
  ,s ++ "eagletmt/ghcmod-vim"                    -- Happy Haskell Programming
  ,s ++ "eagletmt/neco-ghc"                      -- Haskell completion
  ,s ++ "ivalkeen/nerdtree-execute"              -- NERDtree execute menu
  ,s ++ "moll/vim-bbye"                          -- Delete buffers
  ,s ++ "mpickering/hlint-refactor-vim"          -- Hlint refactoring
  ,s ++ "neovimhaskell/haskell-vim"              -- Highlighting & Indentation
  ,s ++ "scrooloose/nerdcommenter"               -- Commenting operators
  ,s ++ "scrooloose/nerdtree"                    -- Filesystem explorer
  ,s ++ "Shougo/neocomplete.vim"                 -- Keyword completion
  ,s ++ "Shougo/vimproc.vim"                     -- Asynchronous execution
  ,s ++ "tpope/vim-commentary"                   -- Lightweight commentary
  ,s ++ "tpope/vim-dispatch"                     -- Background builds
  ,s ++ "tpope/vim-fugitive"                     -- Git interface
  ,s ++ "tpope/vim-pathogen"                     -- Runtime path manager
  ,s ++ "tpope/vim-surround"                     -- Surrounding [], {}, ().
  ,s ++ "tpope/vim-unimpaired"                   -- Complementary pair maps
  ,s ++ "Twinside/vim-hoogle"                    -- Query hoogle
  ,s ++ "vim-scripts/bufexplorer.zip"            -- Buffer explorer
  ,s ++ "vim-scripts/CycleColor"                 -- Cycle through colorsheme
  ,s ++ "traap/vim-dragvisuals"                  -- Drag or Dup visual selection
  ]

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
  setupDirectory bpath
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
