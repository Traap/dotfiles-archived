"base.vim

set nocompatible

source  ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim
runtime ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim

filetype off

execute pathogen#infect()

filetype on
filetype plugin indent on
syntax on
let base16colorspace=256
colorscheme koehler
