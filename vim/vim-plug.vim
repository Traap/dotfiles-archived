" Tell Vim where our plugin manager is located.
source  ~/.vim/autoload/vim-plug/plug.vim
runtime ~/.vim/autoload/vim-plug/plug.vim

" Plugin
call plug#begin('~/.vim/bundle')

" Finders
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ivalkeen/nerdtree-execute'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'haya14busa/incsearch.vim'

" Buffers
Plug 'christoomey/vim-system-copy'
Plug 'moll/vim-bbye'
Plug 'vim-scripts/bufexplorer.zip'
Plug 'vim-utils/vim-most-minimal-folds'
Plug 'tkhoa2711/vim-togglenumber'

" Git
Plug 'int3/vim-extradite'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'vim-scripts/gitignore'

" Bars, pannels and files
Plug 'bling/vim-airline'

" Text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'traap/vim-dragvisuals'
Plug 'junegunn/vim-easy-align'

" Tmux 
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'tmux-plugins/vim-tmux-focus-events'

" Iterm2
Plug 'sjl/vitality.vim'

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

" Checkers (Lint, Syntax, etc.)
 Plug 'vim-syntastic/syntastic'

" Dispatchers
Plug 'tpope/vim-dispatch'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/vimshell.vim'

" Yaml
Plug 'mkasa/neco-yaml-kwalify-schema'
Plug 'Shougo/neocomplete.vim'
Plug 'sunaku/kwalify'

" Help
Plug 'Traap/vim-helptags'
Plug 'beloglazov/vim-online-thesaurus'
Plug 'vim-utils/vim-man'

" Add plugins to runtimepath

call plug#end()
