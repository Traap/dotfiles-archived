" {{{ LEADER SECTION
let mapleader = ","
let g:mapleader = ","
let maplocalleader = ","
set tm=2000                                   " Leader key timeout.
" LEADER SECTION END ------------------------------------------------------- }}}
" {{{ SETTINGS SECTION
" First things ... {{{

" Set things that should come first.
filetype on
filetype plugin indent on
" -------------------------------------------------------------------------- }}}
" {{{ Alphabetical listing of things I set.
set autoindent
set autoread
set autowrite
set cmdheight=1                               " Height of the command bar.
set encoding=utf-8
set fileformats=unix,mac,dos
set history=1000
set matchtime=3
set modelines=0
set mouse=a
set nocompatible
set norelativenumber
set notimeout
set ttimeout
set ttimeoutlen=10
set ttyfast
set virtualedit=block
set tm=2000                                   " Leader key timeout.
nnoremap Q <no>                               " Don't allow Ex mode.
" -------------------------------------------------------------------------- }}}
" {{{ Show trailing whitespaces
set nolist                                    " Show trailing whitespaces
if &listchars ==# 'eol:$'                     " But only interesting whitespace
  set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,trail:-,nbsp:+
endif
" -------------------------------------------------------------------------- }}}
" {{{ Buffer visualization settings
set backspace=indent,eol,start
set colorcolumn=+1
set foldmethod=marker
set laststatus=2
set lazyredraw
set linebreak
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
syntax on
" -------------------------------------------------------------------------- }}}
" {{{ Searching
set hlsearch                                     " Hilight previous search
set ignorecase                                   " Ignore case when searching
set incsearch                                    " Incremental searching
set smartcase                                    " Be smart about case.
" -------------------------------------------------------------------------- }}}
" {{{ The wild, wild, west
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
set wildmenu                                     " Turn on wild ment
set wildmode=list:longest,full                   " Tab0-complete files.
" -------------------------------------------------------------------------- }}}
" {{{ Tabs, spaces and wrapping
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
" {{{ Colors I like using...

if filereadable(expand("~./vimrc_background"))
  let base16colorspace=256
"  set background=dark
  source ~/.vimrc_background
endif

highlight ColorColumn ctermbg=18

" Use same color behind concealed unicode characters
hi clear Conceal
set t_Co=256

" Use visible search hilighting
hi! link Visual Search

" Establish setting for gui and non-gui vim sessions.
if has("gui_running")

  " Set searching colors.
  hi search guifg=#ffffff guibg=#e5786d gui=none
  
  " Don't blink normal mode cursor
  set guicursor=n-v-c:block-Cursor
  set guicursor+=n-v-c:blinkon0

  set guioptions-=T
  set guioptions-=e
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " Set fonts.
  if has("gui_macvim")
    set guifont=Menlo
  else
    set guifont=DejaVu\ Sans\ Mono\ 10
  endif

  set guitablabel=%M\ %t
  set guifont=Menlo:h13
  hi Directory guifg=#8ac6f2

  hi Cursor guibg=cyan
  
else
  hi search ctermfg=white ctermbg=173 cterm=none
endif


" Set utf8 as standard encoding and en_US as the standard language
if !has('nvim')
  " Only set this for vim, since neovim is utf8 as default and setting it
  " causes problems when reloading the .vimrc configuration
  set encoding=utf8
endif

" disable Background Color Erase (BCE) so that color schemes
" render properly when inside 256-color tmux and GNU screen.
" see also http://snk.tuxfamily.org/log/vim-256color-bce.html
if &term =~ '256color'
  set t_ut=
endif
" -------------------------------------------------------------------------- }}}
" {{{ Automatic spelling corrections.

iab liek     like
iab liekwise likewise
iab pritn    print
iab moer     more
iab retrun   return
iab teh      the
" -------------------------------------------------------------------------  }}}
" {{{ Auto commands
autocmd BufRead,BufNewFile *.adoc,*adoci,*.txt,*.asciidoc,README
        \ setlocal filetype=asciidoc
autocmd BufRead,BufNewFile *.tex,*.bbl,*.bib,*.texx,*.texb,*.cls
        \ setlocal filetype=tex
autocmd BufRead,BufNewFile *.csv setlocal nowrap tw=0
" -------------------------------------------------------------------------- }}}
" {{{ Obfuscate screen contents
nnoremap <F1> mzggg?G`z
" -------------------------------------------------------------------------- }}}
" {{{ Delete line and more 

"Delete line
map - dd

" Save file
noremap s :w<cr>

" Reformat lines.
nnoremap Q gqip
vnoremap Q gq

" Substitute
nnoremap <c-s> :%s/
vnoremap <c-s> :s/
" -------------------------------------------------------------------------- }}}
" {{{ Quicker access to Ex commands and sourcing.
nmap ; :
nnoremap <leader>sv :source $MYVIMRC<CR>
" -------------------------------------------------------------------------- }}}
" {{{ Toggle search results
noremap <silent><leader><space> :set hlsearch!<CR>
" -------------------------------------------------------------------------- }}}
" {{{ Sort lines
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>
vnoremap <leader>u :sort u<cr>
" -------------------------------------------------------------------------- }}}
" {{{ Clean trailing whitespace
nnoremap <leader>ww mz:%s/\s\+$//<cr>:let @/=''<cr>`z
" -------------------------------------------------------------------------- }}}
" {{{ Select entire buffer
nnoremap vaa ggvGg_
nnoremap Vaa ggVG
" -------------------------------------------------------------------------- }}}
" {{{ Zoom to head level.
nnoremap zh mzzt10<c-u>`z
" -------------------------------------------------------------------------- }}}
" {{{ Easier linewise reselection of what you just pasted.
nnoremap <leader>V V`]
" -------------------------------------------------------------------------- }}}
" {{{ Indent/dedent/autoindent what you just pasted.
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=
" -------------------------------------------------------------------------  }}}
" {{{ Join line
" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Join an entire paragraph
nnoremap <leader>J mzvipJ`z
" -------------------------------------------------------------------------- }}}
" {{{ Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w
" -------------------------------------------------------------------------- }}}
" {{{ Source lines
vnoremap <leader>S y:@"<CR>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>
" -------------------------------------------------------------------------- }}}
" {{{ Marks and Quotes
noremap ' `
noremap æ '
noremap ` <C-^>
" -------------------------------------------------------------------------- }}}
" {{{ Select (charwise) the contents of the current line, excluding indentation.
nnoremap vv ^vg_
" -------------------------------------------------------------------------- }}}
" {{{ Sudo to write
cnoremap w!! w !sudo tee % >/dev/null
" -------------------------------------------------------------------------- }}}
" {{{ Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>
" -------------------------------------------------------------------------- }}}
" {{{ Redraw my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>
" -------------------------------------------------------------------------- }}}
" {{{ Insert Mode Completion
inoremap <c-f> <c-x><c-f>
inoremap <c-]> <c-x><c-]>
inoremap <c-l> <c-x><c-l>
" -------------------------------------------------------------------------- }}}
" {{{ Execute the current line of text as a shell command.
noremap <leader>E !!$SHELL<cr>
" -------------------------------------------------------------------------- }}}
" {{{ EX | chmod +x " | https://github.com/junegunn/dotfiles/blob/master/vimrc
command! EX if !empty(expand('%'))
  \|   write
  \|   call system('chmod +x '.expand('%'))
  \|   silent e
  \| else
  \|   echohl WarningMsg
  \|   echo 'Save the file first'
  \|   echohl None
  \| endif
" -------------------------------------------------------------------------- }}}
" {{{ <Leader>?/! | Google it / Feeling luckey | junegunn/dotfiles
function! s:goog(pat, lucky)
  let q = '"'.substitute(a:pat, '["\n]', ' ', 'g').'"'
  let q = substitute(q, '[[:punct:] ]',
       \ '\=printf("%%%02X", char2nr(submatch(0)))', 'g')
  call system(printf('open "https://www.google.com/search?%sq=%s"',
                   \ a:lucky ? 'btnI&' : '', q))
endfunction

nnoremap <leader>? :call <SID>goog(expand("<cWORD>"), 0)<cr>
nnoremap <leader>! :call <SID>goog(expand("<cWORD>"), 1)<cr>
xnoremap <leader>? "gy:call <SID>goog(@g, 0)<cr>gv
xnoremap <leader>! "gy:call <SID>goog(@g, 1)<cr>gv
" -------------------------------------------------------------------------- }}}
" {{{ Display help in vertical buffer.
nnoremap <leader>HH :silent vert bo help<cr>
" -------------------------------------------------------------------------- }}}
" {{{ Quick editing of my personalization files.
nnoremap <leader>eS :e ~/git/dotfiles/setup.hs<cr>
nnoremap <leader>ea :e ~/git/dotfiles/alias_and_functions<cr>
nnoremap <leader>ed :e ~/git/dotfiles/vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :e ~/git/dotfiles/gitconfig<cr>
nnoremap <leader>ep :e ~/git/dotfiles/vim/vim-plug.vim<cr>
nnoremap <leader>es :e ~/git/dotfiles/vim/settings.vim<cr>
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>.  :e.<cr>
nnoremap <leader>ad :set filetype=asciidoc<cr>
" -------------------------------------------------------------------------- }}}
" SETTINGS SECTION END ----------------------------------------------------- }}}
" {{{ BUNDLES SECTION
" {{{ Air line

if !exists('g:airline_symbols')               " Use powerline fonts for airline
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

let g:airline_powerline_fonts = 1
let g:airline_symbols.space = "\ua0"

let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '' : 'V',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ }

" -------------------------------------------------------------------------  }}}
" Bbye (Buffer Bye) for Vim {{{
nnoremap <leader>q :Bdelete<cr>
nnoremap <leader>Q :bufdo :Bdelete<cr>
nnoremap <leader>X :bdelete<cr>
" -------------------------------------------------------------------------- }}}
" {{{ CTRL-P
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }

nnoremap <silent> <leader>ff :CtrlP<CR> "Fuzzy find
" -------------------------------------------------------------------------- }}}
" {{{ Drag Visual Block
" Remove any introduced trailing whitespace after moving.
vmap  <expr>  <LEFT>   DVB_Drag('left')
vmap  <expr>  <RIGHT>  DVB_Drag('right')
vmap  <expr>  <DOWN>   DVB_Drag('down')
vmap  <expr>  <UP>     DVB_Drag('up')
vmap  <expr>  D        DVB_Duplicate()
let g:DVB_TrimWS = 1
" -------------------------------------------------------------------------- }}}
" {{{ Dispatch
let g:dispatch_compilers = {
     \ 'pdflatex': 'tex',
     \ 'haskell': 'cabal install'
     \ }
" -------------------------------------------------------------------------- }}}
" {{{ EasyAlign
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign in motino/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" -------------------------------------------------------------------------- }}}
" {{{ Fugitive
nnoremap <leader>gP :Gpush<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gh :silent vert bo help fugitive<cr>
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gp :Gpull<cr>
nnoremap <leader>gs :Gstatus<cr>gg<c-n>
nnoremap <leader>gD :Gvdiff<cr>
" -------------------------------------------------------------------------- }}}
" {{{ ghcmod-vim
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
" {{{ haskell-vim
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
" {{{ Helptags
noremap<leader>ph :Helptags<cr>:echo 'Helptags done!'<cr>
" -------------------------------------------------------------------------- }}}
" {{{ neco-ghc
let g:haskell_completion_ghc = 0              " Disabled for neco-ghc
let g:necoghc_enabled_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
" -------------------------------------------------------------------------- }}}
" {{{ NERDtree
let NERDTreeShowLineNumbers=1
let NERDTreeWinPos=1
nnoremap <silent><leader>nf :NERDTreeFind<CR>
nnoremap <silent><C-n> :NERDTreeToggle<CR>
" -------------------------------------------------------------------------- }}}
" {{{ neocomplete
let g:neocomplete#enable_at_startup = 0
" -------------------------------------------------------------------------- }}}
" {{{ Rainbow
let g:rainbow_active = 1

let rainbow_conf = {
    \   'guifgs':
    \       ['royalblue3'
    \       ,'darkorange3'
    \       ,'seagreen3'
    \       ,'firebrick'
    \       ]
    \,
    \   'ctermfgs':
    \       ['lightblue'
    \       ,'lightyellow'
    \       ,'lightcyan'
    \       ,'lightmagenta'
    \       ]
    \,
    \   'operators': '_,_'
    \,
    \   'parentheses':
    \       ['start=/(/ end=/)/ fold'
    \       ,'start=/\[/ end=/\]/ fold'
    \       ,'start=/{/ end=/}/ fold'
    \       ]
    \,
    \   'separately': {
    \       '*': {}
    \,
    \       'tex': {
    \           'parentheses':
    \                ['start=/(/ end=/)/'
    \                ,'start=/\[/ end=/\]/'
    \                ,'start=/{/ end=/}/'
    \                ]
    \,
    \       },
    \       'lisp': {
    \           'guifgs':
    \                ['royalblue3'
    \                ,'darkorange3'
    \                ,'seagreen3'
    \                ,'firebrick'
    \                ,'darkorchid3'
    \                ]
    \,
    \       },
    \       'vim': {
    \           'parentheses':
    \                ['start=/(/ end=/)/'
    \                ,'start=/\[/ end=/\]/'
    \                ,'start=/{/ end=/}/ fold'
    \                ,'start=/(/ end=/)/ containedin=vimFuncBody'
    \                ,'start=/\[/ end=/\]/ containedin=vimFuncBody'
    \                ,'start=/{/ end=/}/ fold containedin=vimFuncBody'
    \                ]
    \,
    \       },
    \       'html': {
    \           'parentheses':
    \               ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold']
    \,
    \       },
    \       'css': 0,
    \   }
    \}
" -------------------------------------------------------------------------- }}}
" {{{ Tmux Runner
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
" {{{ vim-hoogle
let g:hoogle_search_buf_name = 'HoogleSearch'
let g:hoogle_search_count = 20
au BufNewFile,BufRead *.hs map <buffer> <leader>Hc :Hoogle<cr>
au BufNewFile,BufRead *.hs map <buffer> <leader>Hh :Hoogle<cr>
au BufNewFile,BufRead *.hs map <buffer> <leader>Hl :Hoogle<cr>
" -------------------------------------------------------------------------- }}}
" {{{ vimshell
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_prompt = $USER."$ "
" -------------------------------------------------------------------------- }}}
" {{{ vitality
let g:vitality_fix_cursor = 0
" -------------------------------------------------------------------------- }}}
" {{{ vimtex
let g:tex_flavor = 'latex'
let g:vimtex_complete_enabled = 1
let g:vimtex_disable_version_warning = 1
let g:vimtex_enable = 1
let g:vimtex_fold_comments = 0
let g:vimtex_fold_documentclass = 0
let g:vimtex_fold_enabled = 0
let g:vimtex_fold_envs = 0
let g:vimtex_fold_levelmarker = '*'
let g:vimtex_fold_manual = 0
let g:vimtex_fold_preamble = 0
let g:vimtex_fold_usepackage = 0
let g:vimtex_format_enabled = 1
let g:vimtex_imaps_enabled = 0
let g:vimtex_quickfix_autojump = 0
let g:vimtex_quickfix_ignore_all_warnings = 0
let g:vimtex_quickfix_mode = 2
let g:vimtex_view_enabled = 1
let g:vimtex_latexmk_continuous = 1
let g:vimtex_latexmk_background = 0
" -------------------------------------------------------------------------- }}}
" BUNDLES SECTION END ------------------------------------------------------ }}}
