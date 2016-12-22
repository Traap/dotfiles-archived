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

-- | These are the repos that are cloned.
data Repo = REPO
  {tDir :: String   -- the target directory for the clone operation.
  ,repo :: [String] -- the repositories to clone.
  }

repos :: String -> [REPO]
repos s = 
  [REPO{tdir="vim/autoload"
       ,repo=[s ++ "junegunn/vim-plug"]
       }
  ,REPO{tdir="color"
       ,repo=[s ++ "chriskempson/base16-gnome-terminal"
             ,s ++ "chriskempson/base16-iterm2"
             ,s ++ "chriskempson/base16-shell"
             ]
       }
  ,REPO{tdir="ssh"
       ,repo=[s ++ "traap/ssh"]
       }
  ] 

-- | Let's move some files around.
main :: IO ()
main = do
  -- Step 1: Setup symlinnks.
  mapM_ makeSymbolicLink dotfiles

  -- Step 2: Clone repositories from github.
  mapM withDirCloneRepo (repos github) repos

-- | makeSymoblicLink
makeSymbolicLink :: String -> IO ExitCode
makeSymbolicLink f = do
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ f
  _ <- system $ "rm -rf " ++ tfile

  c <- getCurrentDirectory
  let sfile = c ++ "/" ++ takeFileName f
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | 
withDirCloneRepo :: REPO -> IO ()
withDirCloneRepos r = do
  setupDirectory (tdir r)
  mapM cloneRepo (repo r)

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

