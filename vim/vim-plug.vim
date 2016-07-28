" Tell Vim where our plugin manager is located.
source  ~/.vim/autoload/vim-plug/plug.vim
runtime ~/.vim/autoload/vim-plug/plug.vim

" Plugin
call plug#begin('~/.vim/bundle')

" Support bundles
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'moll/vim-bbye'
Plug 'vim-scripts/gitignore'
Plug 'bruno-/vim-man'
Plug 'vim-scripts/bufexplorer.zip'
Plug 'Traap/vim-helptags'

" Git
Plug 'int3/vim-extradite'
Plug 'tpope/vim-fugitive'

" Bars, pannels and files
Plug 'bling/vim-airline'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ivalkeen/nerdtree-execute'
Plug 'scrooloose/nerdtree'

" Text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'traap/vim-dragvisuals'

" Allow pane movement to move between vim and tmux
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'edkolev/tmuxline.vim'

" Haskell
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'enomsg/vim-haskellConcealPlus', { 'for': 'haskell' }
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

" Colorscheme
Plug 'chriskempson/base16-vim'
Plug 'vim-scripts/CycleColor'

" LaTeX
Plug 'LaTeX-Box-Team/LaTeX-Box'

" Dispatchers
Plug 'tpope/vim-dispatch'

" Add plugins to runtimepath
call plug#end()
