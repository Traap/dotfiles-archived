" base.vim {{{
"
" This file sets up pathogen and turns off vi compatability.
"
" Let the good times roll.

set nocompatible

source  ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim
runtime ~/git/dotfiles/vim/bundle/vim-pathogen/autoload/pathogen.vim

filetype off 
execute pathogen#infect()

" ---------------------------------------------------------------------------}}}
