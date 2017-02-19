-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause
--
-- This program is used to bootstrap a development environment.  Bootstrapping
-- consists of two parts, namely: 1) setup symbolic links to files or
-- directories that are under version control, and 2) clone GitHub.com
-- repositories that are needed to personalize bash and vim.

-- | The main entry point.
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

-- | The SymLink type defines the target of a symlink operation and identifies
-- the target type as a file or as a directory.
data SymLink = SYMLINK
  {sym  :: String -- the target for the Symlink.
  ,flag :: Bool -- true when a file; false when a directory.
  } deriving (Show)

-- | The symlinks function creates a list of SymLinks.  Each SymLink represents
-- a named symbolic link and file or directory target that is symbolically
-- linked.
symlinks :: [SymLink]
symlinks =
  [SYMLINK{sym="bash_profile",     flag=True}
  ,SYMLINK{sym="bashrc",           flag=True}
  ,SYMLINK{sym="gitconfig",        flag=True}
  ,SYMLINK{sym="gitignore_global", flag=True}
  ,SYMLINK{sym="gvimrc",           flag=True}
  ,SYMLINK{sym="inputrc",          flag=True}
  ,SYMLINK{sym="ssh",              flag=False}
  ,SYMLINK{sym="ssh/config",       flag=True}
  ,SYMLINK{sym="tmux",             flag=False}
  ,SYMLINK{sym="tmux.conf",        flag=True}
  ,SYMLINK{sym="vim",              flag=False}
  ,SYMLINK{sym="vimrc",            flag=True}
  ,SYMLINK{sym="vimrc_background", flag=True}
  ]

data Url = URL
  { loc  :: String -- the url string.
  , here :: Bool   -- use . (here) versus url name to clone into.
  } deriving (Show)

-- | The Repo type defines target directory for a git-clone operation and the
-- URL cloned from GitHub.com.
data Repo = REPO
  {tdir :: String   -- the target directory for the clone operation.
  ,url  :: [Url] -- the repositories to clone.
  } deriving (Show)

-- | The repos function creates a list of Repos.  Each Repo represents
-- a repository cloned from GitHub.com.
repos :: String -> [Repo]
repos s =
  [REPO
       {tdir="vim/autoload"
       ,url=[URL{loc=s ++ "junegunn/vim-plug", here=False}]
       }
  ,REPO
       {tdir="color"
       ,url=[URL{loc=s ++ "chriskempson/base16-gnome-terminal", here=False}
            ,URL{loc=s ++ "chriskempson/base16-iterm2", here=False}
            ,URL{loc=s ++ "chriskempson/base16-shell", here=False}
            ]
       }
  ,REPO
       {tdir="ssh"
       ,url=[URL{loc=s ++ "traap/ssh", here=True}]
       }
  ]

-- | Orchestrate cloning GitHub.com repositories and creating symbolic links.
main :: IO ()
main = do
  -- Step 1: Remove everything we are about to create. 
  mapM_ deleteSymLink symlinks

  -- Step 2: Clone repositories from github.
  mapM_ withDirCloneRepo $ repos github

  -- Step 3: Create symbolic links.
  mapM_ makeSymLink symlinks

-- | Recursively delete objects referenced by SymLink. 
deleteSymLink :: SymLink -> IO ExitCode
deleteSymLink sl = do
  -- Concatenate target file name (ex: ~/.bashrc).
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ sym sl

  -- Remove the target file and create the symbolic link.
  system $ "rm -vrf " ++ tfile

-- | Create symbolic links for objects referenced by SymLink. 
makeSymLink :: SymLink -> IO ExitCode
makeSymLink sl = do
  -- Concatenate target file name (ex: ~/.bashrc).
  h <- getHomeDirectory
  let tfile = h ++ "/." ++ sym sl

  -- Concatenate source file name (ex: ~/git/dotfiles/bashrc).
  c <- getCurrentDirectory
  let sfile = c ++ "/" ++ takeFileName (sym sl)

  -- Remove the target file and create the symbolic link.
  _ <- system $ "rm -vrf " ++ tfile
  system $ "ln -vs " ++ sfile ++ " " ++ tfile

-- | Setup directory to clone repository into.
withDirCloneRepo :: Repo -> IO ()
withDirCloneRepo r = do
  setupDirectory (tdir r)
  mapM_ cloneRepo (url r)

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
-- The url cloned has two formats
-- 1) clone git@github.com:Traap/dotfiles
--    This invocation creates a new directory named dotfiles and clones into it.
--
-- 2) clone git@github.com:Traap/dotfiles .
--    This invocation clones into the current directory.
--
cloneRepo :: Url -> IO ExitCode
cloneRepo u = do
  let s = if here u then loc u ++ " ." else loc u
  system s
