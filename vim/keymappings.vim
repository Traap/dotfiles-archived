" Keymappings {{{
" 
" I do not intend to place any keybindings in this file that are bundle 
" specific.
"
" Replace help key with check time.
noremap  <F1> :checktime<cr>
inoremap <F1> <esc>:checktime<cr>
" -------------------------------------------------------------------------- }}}
" Obfuscate screen contents {{{
nnoremap <F9> mzggg?G`z
" -------------------------------------------------------------------------- }}}
" Toggle line numbers {{{
nnoremap <leader>n :set invnumber<cr>
" -------------------------------------------------------------------------- }}}
" Sort lines {{{
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>
" -------------------------------------------------------------------------- }}}
" Wrap {{{
nnoremap <leader>W :set wrap!<cr>
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
" Diffoff {{{
nnoremap <leader>D :diffoff!<cr>
" -------------------------------------------------------------------------- }}}
" Easier linewise reselection of what you just pasted. {{{
nnoremap <leader>V V`]
" -------------------------------------------------------------------------- }}}
" Indent/dedent/autoindent what you just pasted. {{{
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=
" -------------------------------------------------------------------------- }}}
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
" Ranger {{{
nnoremap <leader>r :silent !ranger %:h<cr>:redraw!<cr>
nnoremap <leader>R :silent !ranger<cr>:redraw!<cr>
" -------------------------------------------------------------------------- }}}
" Insert Mode Completion {{{
inoremap <c-f> <c-x><c-f>
inoremap <c-]> <c-x><c-]>
inoremap <c-l> <c-x><c-l>
" -------------------------------------------------------------------------- }}}
" Matching behavior {{{
" Clear matches
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz
" -------------------------------------------------------------------------- }}}
" It's 2013. {{{
noremap j gj
noremap k gk
noremap gj j
noremap gk k
" -------------------------------------------------------------------------- }}}
" execute the current line of text as a shell command. {{{
noremap <leader>E !!$SHELL<cr>
" -------------------------------------------------------------------------- }}}
" Easy buffer navigation {{{
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" -------------------------------------------------------------------------- }}}
" Split buffer {{{
noremap <leader>h :split<cr><cr>
noremap <leader>v <C-w>v
" -------------------------------------------------------------------------- }}}
" List {{{
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz
" -------------------------------------------------------------------------- }}}
" Yank, Delete and Paste (cut-copy-paste) to os clipboard.
nmap <leader>y "*y
vmap <leader>y "*y
nmap <leader>d "*d
vmap <leader>d "*d
nmap <leader>p "*p
vmap <leader>p "*y
" -------------------------------------------------------------------------- }}}
" Display help in vertical buffer. {{{
nnoremap <leader>H :silent vert bo help<cr>
" -------------------------------------------------------------------------- }}}
" Quick editing of my personalization files.{{{
nnoremap <leader>eS :e ~/git/dotfiles/setup.hs<cr>
nnoremap <leader>ea :e ~/git/dotfiles/alias_and_functions<cr>
nnoremap <leader>eb :e ~/git/dotfiles/vim/base.vim<cr>
nnoremap <leader>ed :e ~/git/dotfiles/vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :e ~/git/dotfiles/gitconfig<cr>
nnoremap <leader>ek :e ~/git/dotfiles/vim/keymappings.vim<cr>
nnoremap <leader>el :e ~/git/dotfiles/vim/leader.vim<cr>
nnoremap <leader>ep :e ~/git/dotfiles/vim/bundles.vim<cr>
nnoremap <leader>es :e ~/git/dotfiles/vim/settings.vim<cr>
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>.  :e.<cr>
nnoremap <leader>ad :set filetype=asciidoc<cr>
nnoremap <leader>tw :set textwidth=0 wrapmargin=0 nowrap nogdefault relativenumber invnumber<cr>
" -------------------------------------------------------------------------- }}}
" Escape key and dd {{{
inoremap <tab> <ESC>
map - dd
" -------------------------------------------------------------------------- }}}
" Quicker access to Ex commands {{{
nmap ; :
" -------------------------------------------------------------------------- }}}
