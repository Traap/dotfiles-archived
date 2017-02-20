" Tell Vim where our plugin manager is located.
source  ~/.vim/autoload/vim-plug/plug.vim
runtime ~/.vim/autoload/vim-plug/plug.vim

" Plugin
call plug#begin('~/.vim/bundle')

" Support bundles
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/vimshell.vim'
Plug 'Traap/vim-helptags'
Plug 'beloglazov/vim-online-thesaurus'
Plug 'vim-utils/vim-man'
Plug 'moll/vim-bbye'
Plug 'vim-scripts/bufexplorer.zip'
Plug 'vim-scripts/gitignore'

" Git
Plug 'int3/vim-extradite'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'

" Bars, pannels and files
Plug 'bling/vim-airline'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ivalkeen/nerdtree-execute'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" Text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'traap/vim-dragvisuals'
Plug 'junegunn/vim-easy-align'

" Allow pane movement to move between vim and tmux
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'sjl/vitality.vim'
Plug 'tmux-plugins/vim-tmux-focus-events'

" Haskell
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'enomsg/vim-haskellConcealPlus', { 'for': 'haskell' }
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

" Colors
Plug 'chriskempson/base16-vim'
Plug 'luochen1990/rainbow'

" LaTeX
Plug 'lervag/vimtex'

" Checkers (Lint, Syntat, etc.)
 Plug 'vim-syntastic/syntastic'

" Dispatchers
Plug 'tpope/vim-dispatch'

" Add plugins to runtimepath

call plug#end()
