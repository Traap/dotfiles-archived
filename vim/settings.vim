" {{{ LEADER SECTION
let mapleader = ","
let g:mapleader = ","
let maplocalleader = ","
set tm=2000                                   " Leader key timeout.
" LEADER SECTION END ------------------------------------------------------- }}}
" {{{ SETTINGS SECTION 
" First things ... {{{

" Set things that should come first.  Later autocmd can override the
" result as needed.
filetype on
filetype plugin indent on
" -------------------------------------------------------------------------- }}}
" Alphabetical listing things I set. {{{

set autoindent
set autoread
set autowrite
set encoding=utf-8
set fileformats=unix,mac,dos
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
set virtualedit=block
nnoremap Q <no>                               " Don't allow Ex mode.
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

set hlsearch                                     " Hilight previous search
set ignorecase                                   " Ignore case when searching
set incsearch                                    " Incremental searching
set smartcase                                    " Be smart about case.
" -------------------------------------------------------------------------- }}}
" The wild, wild, west {{{
set wildignore+=*.DS_Store                       " OSX set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
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
set formatoptions=qrn1t
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
set background=dark
"highlight ColorColumn ctermbg=cyan
syntax on
" disable Background Color Erase (BCE) so that color schemes
" render properly when inside 256-color tmux and GNU screen.
" see also http://snk.tuxfamily.org/log/vim-256color-bce.html
if &term =~ '256color'
  set t_ut=
endif
" -------------------------------------------------------------------------- }}}
" Automatic spelling corrections. {{{

iab liek     like
iab liekwise likewise
iab pritn    print
iab moer     more
iab retrun   return
iab teh      the
" -------------------------------------------------------------------------  }}}
" Auto commands {{{
autocmd BufRead,BufNewFile *.adoc,*adoci,*.txt,*.asciidoc,README
        \ setlocal filetype=asciidoc
autocmd BufRead,BufNewFile *.tex,*.bbl,*.bib,*.texx,*.texb
        \ setlocal filetype=tex
" -------------------------------------------------------------------------- }}}
" {{{ Help repaced with check time 
"
noremap  <F1> :checktime<cr>
inoremap <F1> <esc>:checktime<cr>
" -------------------------------------------------------------------------- }}}
" Obfuscate screen contents {{{
nnoremap <F9> mzggg?G`z
" -------------------------------------------------------------------------- }}}
" Sort lines {{{
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>
" -------------------------------------------------------------------------- }}}
" Clean trailing whitespace {{{
nnoremap <leader>ww mz:%s/\s\+$//<cr>:let @/=''<cr>`z
" -------------------------------------------------------------------------- }}}
" Select entire buffer {{{
nnoremap vaa ggvGg_
nnoremap Vaa ggVG
" -------------------------------------------------------------------------- }}}
" zoom to head level. {{{
nnoremap zh mzzt10<c-u>`z
" -------------------------------------------------------------------------- }}}
" Easier linewise reselection of what you just pasted. {{{
nnoremap <leader>V V`]
" -------------------------------------------------------------------------- }}}
" Indent/dedent/autoindent what you just pasted. {{{
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=
" -------------------------------------------------------------------------  }}}
" Join line {{{
" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Join an entire paragraph 
nnoremap <leader>J mzvipJ`z
" -------------------------------------------------------------------------- }}}
" Split line (sister to [J]oin lines) {{{
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w
" -------------------------------------------------------------------------- }}}
" Source lines{{{
vnoremap <leader>S y:@"<CR>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>
" -------------------------------------------------------------------------- }}}
" Marks and Quotes {{{
noremap ' `
noremap æ '
noremap ` <C-^>
" -------------------------------------------------------------------------- }}}
" Select (charwise) the contents of the current line, excluding indentation. {{{
nnoremap vv ^vg_
" -------------------------------------------------------------------------- }}}
" Sudo to write {{{
cnoremap w!! w !sudo tee % >/dev/null
" -------------------------------------------------------------------------- }}}
" Toggle [i]nvisible characters {{{
nnoremap <leader>i :set list!<cr>
" -------------------------------------------------------------------------- }}}
" Redraw my screen {{{
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>
" -------------------------------------------------------------------------- }}}
" execute the current line of text as a shell command. {{{
noremap <leader>E !!$SHELL<cr>
" -------------------------------------------------------------------------- }}}
" Display help in vertical buffer. {{{
nnoremap <leader>HH :silent vert bo help<cr>
" -------------------------------------------------------------------------- }}}
" Quick editing of my personalization files.{{{
nnoremap <leader>eS :e ~/git/dotfiles/setup.hs<cr>
nnoremap <leader>ea :e ~/git/dotfiles/alias_and_functions<cr>
nnoremap <leader>ed :e ~/git/dotfiles/vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :e ~/git/dotfiles/gitconfig<cr>
nnoremap <leader>es :e ~/git/dotfiles/vim/settings.vim<cr>
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>.  :e.<cr>
nnoremap <leader>ad :set filetype=asciidoc<cr>
" -------------------------------------------------------------------------- }}}
" SETTINGS SECTION END ----------------------------------------------------- }}}
" {{{ BUNDLES SECTION 
" Bbye (Buffer Bye) for Vim {{{
nnoremap <leader>q :Bdelete<cr>
nnoremap <leader>Q :bufdo :Bdelete<cr>
nnoremap <leader>X :bdelete<cr>
" -------------------------------------------------------------------------- }}}
" Drag Visual Block {{{
" Remove any introduced trailing whitespace after moving.
vmap  <expr>  <LEFT>   DVB_Drag('left')
vmap  <expr>  <RIGHT>  DVB_Drag('right')
vmap  <expr>  <DOWN>   DVB_Drag('down')
vmap  <expr>  <UP>     DVB_Drag('up')
vmap  <expr>  D        DVB_Duplicate()
let g:DVB_TrimWS = 1
" -------------------------------------------------------------------------- }}}
" Dispatch {{{
let g:dispatch_compilers = {
     \ 'pdlatex': 'tex',
     \ 'haskell': 'cabal install'
     \ }
" -------------------------------------------------------------------------- }}}
" Fugitive {{{
nnoremap <leader>gP :Gpush<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gh :silent vert bo help fugitive<cr>
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gp :Gpull<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gD :Gvdiff<cr>
" -------------------------------------------------------------------------- }}}
" Pathogen {{{
noremap<leader>ph :Helptags<cr>:echo 'Helptags done!'<cr>
" -------------------------------------------------------------------------- }}}
" NERDtree {{{
let NERDTreeShowLineNumbers=1
nnoremap <silent><leader>nf :NERDTreeFind<CR>
nnoremap <silent><C-n> :NERDTreeToggle<CR>
" -------------------------------------------------------------------------- }}}
" Tmux Runner {{{
"
" Below are the suggested Tmux Runner default mappings.  I decided to explicitly
" reference them with defaults before I start changing them.
nnoremap <leader>rr  :VtrResizeRunner<cr>
nnoremap <leader>ror :VtrReorientRunner<cr>
nnoremap <leader>sc  :VtrSendCommandToRunner<cr>
nnoremap <leader>sf  :VtrSendFile!<cr>
nnoremap <leader>sl  :VtrSendLineToRunner<cr>
nnoremap <leader>or  :VtrOpenRunner<cr>
nnoremap <leader>kr  :VtrKillRunner<cr>
nnoremap <leader>fr  :VtrFocusRunner<cr>
nnoremap <leader>dr  :VtrDetachRunner<cr>
nnoremap <leader>ar  :VtrReattachRunner<cr>
nnoremap <leader>cr  :VtrClearRunner<cr>
nnoremap <leader>fc  :VtrFlushCommand<cr>
let g:vtr_filetype_runner_overrides = {
    \  'asciidoc': 'apdf {file}'
    \ ,'haskell': 'runhaskell {file}'
    \ }

let g:VtrOrientation = "h"
let g:VtrPercentage = 50

nnoremap <leader>db  :VtrSendCommand stack exec -- doc-build<cr>
nnoremap <leader>dc  :VtrSendCommand stack exec -- doc-build clean<cr>
nnoremap <leader>hb  :VtrSendCommand stack build hmst-documentation<cr>
nnoremap <leader>mb  :VtrSendCommand stack exec -- math-build<cr>
nnoremap <leader>mc  :VtrSendCommand stack exec -- math-build clean<cr>
" -------------------------------------------------------------------------- }}}
" LaTex-Box {{{
let g:LatexBox_latexmk_async = 0
let g:LatexBox_quickfix = 1
let g:LatexBox_split_length = 15
let g:LatexBox_Folding = 1
let g:LatexBox_fold_preambel = 0
"let g:LatexBox_latexmk_options = '-pdflatex="pdflatex -synctex=1 %O %S"'
nnoremap <leader>xo <c-x><c-o>
" -------------------------------------------------------------------------- }}}
" CTRL-P {{{
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }

nnoremap <silent> <leader>tp :CtrlP<CR>
nnoremap <silent> <leader>Tp :CtrlPBuffer<CR>
" -------------------------------------------------------------------------- }}}
" haskell-vim{{{
let g:haskell_enable_quantification = 1       " Highlite forall
let g:haskell_enable_recursivedo = 1          " Highlite mdo and rec
let g:haskell_enable_arrowsyntax = 1          " Highlite proc
let g:haskell_enable_pattern_synonyms = 1     " Highlite pattern
let g:haskell_enable_typeroles = 1            " Highlite type roles
let g:haskell_enable_static_pointers = 1      " Highlite static
let g:haskell_indent_if = 3
let g:haskell_indent_case = 2
let g:haskell_indent_let = 4
let g:haskell_indent_where = 6
let g:haskell_indent_do = 3
let g:haskell_indent_int = 1
let g:haskell_indent_guard = 2
let g:cabal_indent_selection = 2
" -------------------------------------------------------------------------- }}}
" neco-ghc {{{
let g:haskell_completion_ghc = 0              " Disabled for neco-ghc
let g:necoghc_enabled_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
" -------------------------------------------------------------------------- }}}
" neocomplete {{{ 
let g:neocomplete#enable_at_startup = 0
" -------------------------------------------------------------------------- }}}
" ghcmod-vim {{{ 
" https://github.com/eagletmt/ghcmod-vim/wiki/Customize
let &l:statusline = '%{empty(getqflist()) ? "[No Errors]" : "[Errors Found]"}' . (empty(&l:statusline) ? &statusline : &l:statusline)

let g:ghcmod_hlint_options = ['--ignore=Redundant $']
let g:ghcmod_type_highlight = 'ghcmodType'

highlight ghcmodtype ctermbg=yellow

nnoremap <silent> tw :GhcModTypeInsert<CR>
nnoremap <silent> ts :GhcModSplitFunCase<CR>
nnoremap <silent> tq :GhcModType<CR>
nnoremap <silent> te :GhcModTypeClear<CR>

autocmd BufWritePost *.hs GhcModCheckAndLintAsync
" -------------------------------------------------------------------------- }}}
" vim-hoogle {{{ 
let g:hoogle_search_count = 20
let g:hoogle_search_buf_name = 'HoogleSearch'

au BufNewFile,BufRead *.hs map <buffer> <leader>Hh :Hoogle<cr>
au BufNewFile,BufRead *.hs map <buffer> <leader>Hc :Hoogle<cr>
au BufNewFile,BufRead *.hs map <buffer> <leader>Hl :Hoogle<cr>
" -------------------------------------------------------------------------- }}}
"i BUNDLES SECTION END ------------------------------------------------------ }}}
