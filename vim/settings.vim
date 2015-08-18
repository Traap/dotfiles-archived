" Set things that should come first.  Later autocmd can override the
" result as needed.
colorscheme koehler
filetype on
filetype plugin indent on
hi colorcolumn ctermbg=blue
let base16colorspace=256
syntax on

" Alphabetical listinf things I set."
set autoread
set autowrite
set backspace=indent,eol,start
set colorcolumn=+1
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview
set encoding=utf-8
set expandtab
set formatoptions=qrn1
set gdefault
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set linebreak
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set matchtime=3
set modelines=0
set nocompatible
set nolist
set norelativenumber
set noswapfile
set notimeout
set number
set numberwidth=1
set ruler
set scrolloff=3
set shiftround
set shiftwidth=4
set showbreak=↪
set showcmd
set showmatch
set showmode
set sidescroll=1
set sidescrolloff=10
set smartcase
set smarttab
set softtabstop=4
set splitbelow
set splitright                                   " New windows open to the right ot the current one
set synmaxcol=800
set tabstop=8
set textwidth=74
set title
set ttimeout
set ttimeoutlen=10
set ttyfast
set undofile
set undoreload=10000
set virtualedit+=block
set visualbell
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
set wrap
