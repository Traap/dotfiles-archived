" Line Return
"
" Make sure Vim returns to the same line when you reopen a file.
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" Jumping to tags.
"
" Basically, <c-]> jumps to tags and <c-\> opens the tag in a new
" split instead.
"
" Both of them will align the destination line to the upper middle part of the
" screen.  Both will pulse the cursor line so you can see where are.  <c-\> 
" will also fold everything in the buffer and then unfold just enough for you
" to see the destination line.
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

" Visual Mode */# from Scrooloose 
function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>

" Backup files
set backup                        " enable backups
set noswapfile                    " it's 2013, Vim.

set undodir=~/.vim/tmp/undo       " undo files
set backupdir=~/.vim/tmp/backup   " backups
set directory=~/.vim/tmp/swap     " swap files

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
