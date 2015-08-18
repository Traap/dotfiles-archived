"base.vim

set nocompatible

source  ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim
runtime ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim

filetype off

execute pathogen#infect()
