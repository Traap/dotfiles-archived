dotfiles
========
I began learning the Haskell programming language starting 2014.  During my 
journey, I have had to learn to configure Haskell and Cabal on multiple 
operating systems:  Windows, OSX, and Linux Mint.  Often I cycled through each 
operating system multiple times daily.  Lacking a common editor was what caused 
me to learn Vim.  I am by no means an expert.  I have read blogs, books, and 
studied how others have organized their dotfiles.  And decided to jump in.

## Installation
My installation instructions are geared toward Haskell and OSX. 
* git clone https://github.com/Traap/dotfiles.git
* cd dotfiles
* runhaskell setup.hs


## Note:
setup.hs creates symbolic links to files in my git/dotfile folde, and then
clones Vim bundles into my git/dotfiles/vim/bundles folder.  I have found
this strategy useful to manage my source files a sinle location versus copying
them to my home directory.


Traap
