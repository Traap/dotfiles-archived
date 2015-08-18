" Keymappings

" Replace help key with check time.
noremap  <F1> :checktime<cr>
inoremap <F1> <esc>:checktime<cr>

" Obfuscate screen contents
nnoremap <f9> mzggg?G`z

" Toggle line numbers
nnoremap <leader>n :set invnumber<cr>

" Sort lines
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>

" Tabs
nnoremap <leader>( :tabprev<cr>
nnoremap <leader>) :tabnext<cr>

" Wrap
nnoremap <leader>W :set wrap!<cr>

" Clean trailing whitespace
nnoremap <leader>ww mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Select entire buffer
nnoremap vaa ggvGg_
nnoremap Vaa ggVG

" zoom to head level.
nnoremap zh mzzt10<c-u>`z

" Diffoff
nnoremap <leader>D :diffoff!<cr>

" Easier linewise reselection of what you just pasted.
nnoremap <leader>V V`]

" Indent/dedent/autoindent what you just pasted.
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Join an entire paragraph.
nnoremap <leader>J mzvipJ`z

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" Source
vnoremap <leader>S y:@"<CR>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" Marks and Quotes
noremap ' `
noremap æ '
noremap ` <C-^>

" Select (charwise) the contents of the current line, excluding indentation.
nnoremap vv ^vg_

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null

" Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>

" Redraw my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" Ranger
nnoremap <leader>r :silent !ranger %:h<cr>:redraw!<cr>
nnoremap <leader>R :silent !ranger<cr>:redraw!<cr>

" Insert Mode Completion
inoremap <c-f> <c-x><c-f>
inoremap <c-]> <c-x><c-]>
inoremap <c-l> <c-x><c-l>

noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

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

" Escape key
inoremap jk <ESC>

" List
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

" Display help in vertical buffer.
nnoremap <leader>h :silent vert bo help<cr>

"Bbye (Buffer Bye) for Vim
nnoremap <leader>q :Bdelete<cr>
nnoremap <leader>Q :bufdo :Bdelete<cr>

"Delete all buffer"
nnoremap <leader>X :bdelete<cr>

"NERDTree
nnoremap <C-n> :NERDTreeToggle<cr>
noremap <leader>nt  :NERDTreeToggle<cr>

"Fugitive"
nnoremap <leader>gP :Gpush<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gh :silent vert bo help fugitive<cr>
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gp :Gpull<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gD :Gvdiff<cr>

"Pathogen"
noremap<leader>ph :Helptags<cr>:echo 'Helptags done!'<cr>

" Quick editing
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>eb :e ~/git/dotfiles/vim/base.vim<cr>
nnoremap <leader>ed :e ~/git/dotfiles/vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>ef :e ~/git/dotfiles/vim/functions.vim<cr>
nnoremap <leader>ek :e ~/git/dotfiles/vim/keymappings.vim<cr>
nnoremap <leader>el :e ~/git/dotfiles/vim/leader.vim<cr>
nnoremap <leader>es :e ~/git/dotfiles/vim/settings.vim<cr>
nnoremap <leader>eu :e ~/git/dotfiles/vim/utilities.vim<cr>
nnoremap <leader>e. :e ~/git/dotfiles/bash_profile<cr>
nnoremap <leader>ea :e ~/git/dotfiles/alias_and_functions<cr>
nnoremap <leader>eg :e ~/git/dotfiles/gitconfig<cr>
nnoremap <leader>eS :e ~/git/dotfiles/setup.hs<cr>
nnoremap <leader>eB :e ~/git/dotfiles/vim/getbundles.hs<cr>
