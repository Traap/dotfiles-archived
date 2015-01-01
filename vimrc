" Pramble ------------------------------------------------------------------ {{{
" .vimrc
" Author: gary.a.howard@mac.com
" Source: https://github.com/Traap/dotfiles
" Acknowledgements:
"   Steve Lash <steve@steveLash.com>
"     http://bitbucket.org/sjl/dotfiles
"     hg clone https://bitbucket.org/sjl/d
" }}}

" Backups ------------------------------------------------------------------ {{{
set backup
set backupdir=~/.vim/tmp/backup/
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim/tmp/swap/ 
set noswapfile
set undodir=~/.vim/tmp/undo/

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
" }}}

" Basic Operations --------------------------------------------------------- {{{
autocmd GUIEnter * set visualbell t_vb=
colorscheme murphy
filetype indent on
set cursorline
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview
set hlsearch
set incsearch
set lazyredraw
set noerrorbells visualbell t_vb=
set noswapfile
set notimeout
set number
set ruler
set runtimepath^=~/.vim/bundle/ctrlp.vim
set showcmd
set splitbelow
set splitright
set synmaxcol=800
set title
set ttimeout
set ttimeoutlen=10
set writebackup
let mapleader = ","
let maplocalleader = "\\"
" }}}

" Keybinding --------------------------------------------------------------- {{{
" Disable F1 help key.
noremap  <F1> :checktime<cr>
inoremap <F1> <esc>:checktime<cr>
map <F2> a<C-R>=strftime("%c")<CR><Esc>

" Sort lines
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>

" Tabs
nnoremap <leader>( :tabprev<cr>
nnoremap <leader>) :tabnext<cr>

" Toggle line numbers
nnoremap <leader>n :setlocal number!<cr>

" Uppercase word
inoremap <C-u> <esc>mzgUiw`za

" Wrap
nnoremap <leader>W :set wrap!<cr>

" Reformat line.
" I never use l as a macro register anyway.
nnoremap ql gqq

" Easier linewise reselection of what you just pasted.
nnoremap <leader>V V`]

" Indent/dedent/autoindent what you just pasted.
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Join an entire paragraph.
"
" Useful for writing GitHub comments in actual Markdown and then translating it
" to their bastardized version of Markdown.
nnoremap <leader>J mzvipJ`z

" HTML tag closing
inoremap <C-_> <space><bs><esc>:call InsertCloseTag()<cr>a

" Source
vnoremap <leader>S y:@"<CR>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>


" Copying text to the system clipboard.
" For some reason Vim no longer wants to talk to the OS X pasteboard through "*.
function! g:CopyToSystemClipboad()
    let old_z = @z
    normal! gv"zy
    call system('pbcopy', @z)
    let @z = old_z
endfunction

noremap <leader>p :silent! set paste<CR>"*p:set nopaste<CR>
vnoremap <leader>y :<c-u>call g:CopyToSystemClipboad()<cr>
nnoremap <leader>y VV:<c-u>call g:CopyToSystemClipboad()<cr>

" Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>

" Sudo to writeAmgen
cnoremap w!! w !sudo tee % >/dev/nullAmgen

" Insert Mode Completion
inoremap <c-f> <c-x><c-f>
inoremap <c-]> <c-x><c-]>
inoremap <c-l> <c-x><c-l>
" }}}

" Wildmenu completion ------------------------------------------------------ {{{
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX spotlight marker fil
set wildignore+=*.luac                           " Lua byte code
" }}}

" Tabs, spaces, wrapping --------------------------------------------------- {{{
set colorcolumn=+1
set expandtab
set formatoptions=qrn1j
set shiftwidth=2
set softtabstop=2
set tabstop=2
set textwidth=80
set wrap
" }}}

" Quick editing ------------------------------------------------------------ {{{
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>ed :vsplit ~/.vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :vsplit ~/.gitconfig<cr>
" }}}

" Directional Keys --------------------------------------------------------- {{{
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" Easy buffer navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

noremap <leader>v <C-w>v
" }}}

" Folding ------------------------------------------------------------------ {{{
set foldlevelstart=0

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" Make zO recursively open whatever fold we're in, even if it's partially open.
nnoremap zO zczO

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
nnoremap <c-z> mzzMzvzz15<c-e>`z:Pulse<cr>

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()

" }}}
