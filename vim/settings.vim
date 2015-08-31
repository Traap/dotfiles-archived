" settings.vim {{{
" 
" These are general settings that impact Vim as a whole.
" -------------------------------------------------------------------------- }}}
" First things ... {{{
" Set things that should come first.  Later autocmd can override the
" result as needed.
filetype on
filetype plugin indent on
" -------------------------------------------------------------------------- }}}
" Alphabetical listinf things I set. {{{
set encoding=utf-8
set gdefault
set hidden
set history=1000
set matchtime=3
set modelines=0
set nocompatible
set nolist
set norelativenumber
set notimeout
set ttimeout
set ttimeoutlen=10
set ttyfast
setlocal spell spelllang=en_us
" -------------------------------------------------------------------------- }}}
" Buffer visualization settings {{{
set backspace=indent,eol,start
set colorcolumn=+1
set foldmethod=marker
set laststatus=2
set lazyredraw
set linebreak
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set number
set numberwidth=1
set ruler
set scrolloff=3
set showbreak=↪
set showcmd
set showmatch
set showmode
set sidescroll=1
set sidescrolloff=10
set splitbelow
set synmaxcol=800
set title
set virtualedit+=block
set visualbell
" -------------------------------------------------------------------------- }}}
" Searching {{{
set hlsearch
set ignorecase
set incsearch
set smartcase
" -------------------------------------------------------------------------- }}}
" The wild, wild, west {{{
set wildignore+=*.DS_Store                       " OSX 
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.orig                           " Merge resolution files
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=migrations                       " Django migrations
set wildmenu
set wildmode=list:longest
" -------------------------------------------------------------------------- }}}
" Tabs, spaces and wrapping {{{
set expandtab
set formatoptions=qrn1
set shiftround
set shiftwidth=2
set smarttab
set softtabstop=2
set tabstop=2
set textwidth=80
set wrap
" ---------------------------------------------------------------------------}}}
" Colors I like using... {{{
let base16colorspace=256
colorscheme desert
syntax on
hi colorcolumn ctermbg=blue
" -------------------------------------------------------------------------- }}}
" Automatic spelling corrections. {{{
iab liek     like
iab liekwise likewise
iab pritn    print
iab moer     more
iab retrun   return
iab teh      the
" -------------------------------------------------------------------------- }}}
" Candidates for removal {{{
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview
" -------------------------------------------------------------------------- }}}
