" Acknowledgement --------------------------------------------------------- {{{
" .vimrc
" Author: gary.a.howard@mac.com
" Source: https://github.com/Traap/dotfiles
"
" Influenced by:
"    Steve Losh <steve@stevelosh.com>
"      http://bitbucket.org/sjl/dotfiles/src/tip/vim/
" a few minutes to kill.
"}}}
" Preamble ---------------------------------------------------------------- {{{
" Basically, I want to set this to a non-login, non-interactive bash shell.
" Using a login/interactive bash as Vim's 'shell' breaks subtle things, like
" ack.vim's command-line argument parsing.  However, I *do* want bash to load
" ~/.bash_profile so my aliases get loaded and such.
set shell=/bin/bash\ --login
filetype off
call pathogen#infect()
filetype plugin indent on
set nocompatible
" }}}
" Basic options ----------------------------------------------------------- {{{
set autoindent
set autoread
set autowrite
set backspace=indent,eol,start
set colorcolumn=+1
set encoding=utf-8
set hidden
set history=1000
set laststatus=2
set lazyredraw
set linebreak
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set matchtime=3
set modelines=0
set number
set norelativenumber
set ruler
set shiftround
set showbreak=↪
set showcmd
set showmode
set splitbelow
set splitright
set title
set ttyfast
set undofile
set undoreload=10000
set visualbell
" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Better Completion
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview

" Save when losing focus
au FocusLost * :silent! wall

" Resize splits when the window is resized
au VimResized * :wincmd =

" Leader
let mapleader = ","
let maplocalleader = "\\"
" }}}
" Spelling ---------------------------------------------------------------- {{{
"
" There are three dictionaries I use for spellchecking:
"
"   /usr/share/dict/words
"   Basic stuff.
"
"   ~/.vim/custom-dictionary.utf-8.add
"   Custom words (like my name).  This is in my (version-controlled) dotfiles.

"   ~/.vim-local-dictionary.utf-8.add
"   More custom words.  This is *not* version controlled, so I can stick
"   work stuff in here without leaking internal names.
"
" I also remap zG to add to the local dict (vanilla zG is useless anyway).
set dictionary=/usr/share/dict/words
set spellfile=~/.vim/custom-dictionary.utf-8.add,~/.vim-local-dictionary.utf-8.add
nnoremap zG 2zg
" }}}
" Cursorline -------------------------------------------------------------- {{{
" Only show cursorline in the current window and in normal mode.
augroup cline
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" }}}
" Trailing whitespace ----------------------------------------------------- {{{
" Only shown when not in insert mode so I don't go insane.

augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:⌴
    au InsertLeave * :set listchars+=trail:⌴
augroup END

" }}}
" Wildmenu completion ----------------------------------------------------- {{{
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX 
set wildignore+=*.luac                           " Lua byte code
set wildignore+=migrations                       " Django migrations
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files

" Clojure/Leiningen
set wildignore+=classes
set wildignore+=lib

" }}}
" Line Return ------------------------------------------------------------- {{{
" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}
" Tabs, spaces, wrapping -------------------------------------------------- {{{
set tabstop=8
set shiftwidth=4
set softtabstop=4
set expandtab
set wrap
set textwidth=80
set formatoptions=qrn1j
set colorcolumn=+1

" }}}
" Backups ----------------------------------------------------------------- {{{
set backup                        " enable backups
set noswapfile                    " it's 2013, Vim.

set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files

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
" Color scheme ------------------------------------------------------------ {{{
syntax on
set background=dark
let g:blue_tabline = 2
let g:blue_html_link_underline = 0
colorscheme murphy

" Reload the colorscheme whenever we write the file.
augroup color_blue_dev
    au!
    au BufWritePost blue.vim color blue
augroup END

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}
" Convenience mappings ---------------------------------------------------- {{{
" Replace help key with check time.
noremap  <F1> :checktime<cr>
inoremap <F1> <esc>:checktime<cr>

" Toggle line numbers
nnoremap <leader>n :setlocal number!<cr>

" Sort lines
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>

" Tabs
nnoremap <leader>( :tabprev<cr>
nnoremap <leader>) :tabnext<cr>

" Wrap
nnoremap <leader>W :set wrap!<cr>

" Copying text to the system clipboard.
function! g:FuckingCopyTheTextPlease()
    let old_z = @z
    normal! gv"zy
    call system('pbcopy', @z)
    let @z = old_z
endfunction

noremap <leader>p :silent! set paste<CR>"*p:set nopaste<CR>
vnoremap <leader>y :<c-u>call g:FuckingCopyTheTextPlease()<cr>
nnoremap <leader>y VV:<c-u>call g:FuckingCopyTheTextPlease()<cr>

" Rebuild Ctags (mnemonic RC -> CR -> <cr>)
nnoremap <leader><cr> :silent !myctags >/dev/null 2>&1 &<cr>:redraw!<cr>

" Highlight Group(s)
nnoremap <F8> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
                        \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
                        \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Clean trailing whitespace
nnoremap <leader>ww mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Select entire buffer
nnoremap vaa ggvGg_
nnoremap Vaa ggVG

" "Uppercase word" mapping.
"
" This mapping allows you to press <c-u> in insert mode to convert the current
" word to uppercase.  It's handy when you're writing names of constants and
" don't want to use Capslock.
"
" To use it you type the name of the constant in lowercase.  While your
" cursor is at the end of the word, press <c-u> to uppercase it, and then
" continue happily on your way:
"
"                            cursor
"                            v
"     max_connections_allowed|
"     <c-u>
"     MAX_CONNECTIONS_ALLOWED|
"                            ^
"                            cursor
"
" It works by exiting out of insert mode, recording the current cursor location
" in the z mark, using gUiw to uppercase inside the current word, moving back to
" the z mark, and entering insert mode again.
"
" Note that this will overwrite the contents of the z mark.  I never use it, but
" if you do you'll probably want to use another mark.
inoremap <C-u> <esc>mzgUiw`za

" Panic Button
nnoremap <f9> mzggg?G`z

" zoom to head level.
nnoremap zh mzzt10<c-u>`z

" Diffoff
nnoremap <leader>D :diffoff!<cr>

" Formatting, TextMate-style
nnoremap Q gqip
vnoremap Q gq

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

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" HTML tag closing
inoremap <C-_> <space><bs><esc>:call InsertCloseTag()<cr>a

" Source
vnoremap <leader>S y:@"<CR>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" Marks and Quotes
noremap ' `
noremap æ '
noremap ` <C-^>

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null

" Typos
command! -bang E e<bang>
command! -bang Q q<bang>
command! -bang W w<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
command! -bang Wa wa<bang>
command! -bang WA wa<bang>
command! -bang Wq wq<bang>
command! -bang WQ wq<bang>

" I suck at typing.
vnoremap - =

" Toggle paste
" For some reason pastetoggle doesn't redraw the screen (thus the status bar
" doesn't change) while :set paste! does, so I use that instead.
" set pastetoggle=<F6>
nnoremap <F6> :set paste!<cr>

" Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>

" Redraw my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" Pushing
nnoremap <leader>Go :Start! git push origin<cr>
nnoremap <leader>Gu :Start! git push upstream<cr>
nnoremap <leader>Hd :Start! hg push default<cr>
nnoremap <leader>Hu :Start! hg push upstream<cr>

" Zip Right
"
" Moves the character under the cursor to the end of the line.  Handy when you
" have something like:
"
"     foo
"
" And you want to wrap it in a method call, so you type:
"
"     println()foo
"
" Once you hit escape your cursor is on the closing paren, so you can 'zip' it
" over to the right with this mapping.
"
" This should preserve your last yank/delete as well.
nnoremap zl :let @z=@"<cr>x$p:let @"=@z<cr>

" Ranger
nnoremap <leader>r :silent !ranger %:h<cr>:redraw!<cr>
nnoremap <leader>R :silent !ranger<cr>:redraw!<cr>

" Jump (see the J mini-plugin later on)
nnoremap <leader>J :J<space>

" Insert Mode Completion -------------------------------------------------- {{{
inoremap <c-f> <c-x><c-f>
inoremap <c-]> <c-x><c-]>
inoremap <c-l> <c-x><c-l>
" }}}
" }}}
" Quick editing ----------------------------------------------------------- {{{
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>ef :vsplit ~/.config/fish/config.fish<cr>
nnoremap <leader>ed :vsplit ~/.vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :vsplit ~/.gitconfig<cr>
" }}}
" Searching and movement -------------------------------------------------- {{{
set gdefault
set hlsearch
set ignorecase
set incsearch
set scrolloff=3
set showmatch
set sidescroll=1
set sidescrolloff=10
set smartcase
set virtualedit+=block

noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim
map <tab> %
silent! unmap [
silent! unmap ]%

" Jumping to tags.
"
" Basically, <c-]> jumps to tags (like normal) and <c-\> opens the tag in a new
" split instead.
"
" Both of them will align the destination line to the upper middle part of the
" screen.  Both will pulse the cursor line so you can see where the hell you
" are.  <c-\> will also fold everything in the buffer and then unfold just
" enough for you to see the destination line.
function! JumpToTag()
    execute "normal! \<c-]>mzzvzz15\<c-e>"
    execute "keepjumps normal! `z"
    Pulse
endfunction
function! JumpToTagInSplit()
    execute "normal! \<c-w>v\<c-]>mzzMzvzz15\<c-e>"
    execute "keepjumps normal! `z"
    Pulse
endfunction
nnoremap <c-]> :silent! call JumpToTag()<cr>
nnoremap <c-\> :silent! call JumpToTagInSplit()<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz
" Directional Keys {{{

" It's 2013.
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
" Visual Mode */# from Scrooloose ----------------------------------------- {{{
function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>
" }}}
" List navigation --------------------------------------------------------- {{{
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz
" }}}
" }}}
" Folding ----------------------------------------------------------------- {{{
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
function! MyFoldText() " -------------------------------------------------- {{{
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
" Firefox ----------------------------------------------------------------- {{{
augroup ft_firefox
    au!
    au BufRead,BufNewFile ~/Library/Caches/*.html setlocal buftype=nofile
augroup END
" }}}
" Haskell ----------------------------------------------------------------- {{{
augroup ft_haskell
    au!
    au BufEnter *.hs compiler ghc
augroup END
" }}}
" Standard In ------------------------------------------------------------- {{{
augroup ft_stdin
    au!
    " Treat buffers from stdin (e.g.: echo foo | vim -) as scratch.
    au StdinReadPost * :set buftype=nofile
augroup END
" }}}
" Vim --------------------------------------------------------------------- {{{
augroup ft_vim
    au!
    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END
" }}}
" Plugin settings --------------------------------------------------------- {{{
" Haskellmode ------------------------------------------------------------- {{{
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
let g:ghc = "/usr/local/bin/ghc"
" }}}
" HTML5 ------------------------------------------------------------------- {{{
let g:event_handler_attributes_complete = 0
let g:rdfa_attributes_complete = 0
let g:microdata_attributes_complete = 0
let g:atia_attributes_complete = 0
" }}}
" Linediff ---------------------------------------------------------------- {{{
vnoremap <leader>l :Linediff<cr>
nnoremap <leader>L :LinediffReset<cr>
" }}}
" Makegreen --------------------------------------------------------------- {{{
nnoremap \| :call MakeGreen('')<cr>
" }}}
" NERD Tree --------------------------------------------------------------- {{{
noremap  <F2> :NERDTreeToggle<cr>
inoremap <F2> <esc>:NERDTreeToggle<cr>

augroup ps_nerdtree
    au!

    au Filetype nerdtree setlocal nolist
    au Filetype nerdtree nnoremap <buffer> H :vertical resize -10<cr>
    au Filetype nerdtree nnoremap <buffer> L :vertical resize +10<cr>
    " au Filetype nerdtree nnoremap <buffer> K :q<cr>
augroup END

let NERDTreeHighlightCursorline = 1
let NERDTreeIgnore = ['.vim$', '\~$', '.*\.pyc$', 'pip-log\.txt$', 'whoosh_index',
                    \ 'xapian_index', '.*.pid', 'monitor.py', '.*-fixtures-.*.json',
                    \ '.*\.o$', 'db.db', 'tags.bak', '.*\.pdf$', '.*\.mid$',
                    \ '.*\.midi$']

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDChristmasTree = 1
let NERDTreeChDirMode = 2
let NERDTreeMapJumpFirstChild = 'gK'
" }}}
" Powerline --------------------------------------------------------------- {{{
let g:Powerline_symbols = 'fancy'
let g:Powerline_cache_enabled = 1
let g:Powerline_colorscheme = 'red'
" }}}
" Scratch ----------------------------------------------------------------- {{{
command! ScratchToggle call ScratchToggle()

function! ScratchToggle()
    if exists("w:is_scratch_window")
        unlet w:is_scratch_window
        exec "q"
    else
        exec "normal! :Sscratch\<cr>\<C-W>L"
        let w:is_scratch_window = 1
    endif
endfunction

nnoremap <silent> <leader><tab> :ScratchToggle<cr>
" }}}
" }}}
" Environments (GUI/Console) ---------------------------------------------- {{{
if has('gui_running')
    " GUI Vim

    set guifont=Menlo\ Regular\ for\ Powerline:h12

    " Remove all the UI cruft
    set go-=T
    set go-=l
    set go-=L
    set go-=r
    set go-=R

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Different cursors for different modes.
    set guicursor=n-c:block-Cursor-blinkon0
    set guicursor+=v:block-vCursor-blinkon0
    set guicursor+=i-ci:ver20-iCursor

    if has("gui_macvim")
        " Full screen means FULL screen
        set fuoptions=maxvert,maxhorz

        " Use the normal HIG movements, except for M-Up/Down
        let macvim_skip_cmd_opt_movement = 1
        no   <D-Left>       <Home>
        no!  <D-Left>       <Home>
        no   <M-Left>       <C-Left>
        no!  <M-Left>       <C-Left>

        no   <D-Right>      <End>
        no!  <D-Right>      <End>
        no   <M-Right>      <C-Right>
        no!  <M-Right>      <C-Right>

        no   <D-Up>         <C-Home>
        ino  <D-Up>         <C-Home>
        imap <M-Up>         <C-o>{

        no   <D-Down>       <C-End>
        ino  <D-Down>       <C-End>
        imap <M-Down>       <C-o>}

        imap <M-BS>         <C-w>
        inoremap <D-BS>     <esc>my0c`y
    else
        " Non-MacVim GUI, like Gvim
    end
else
    " Console Vim
    " For me, this means iTerm2, possibly through tmux

    " Mouse support
    set mouse=a
endif
" }}}
