" bundles.vim {{{
"
" This file holds bundle specific settings and key bindings.
"
" -------------------------------------------------------------------------- }}}
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
noremap<C-n> :NERDTreeToggle<cr>
let NERDTreeShowLineNumbers=1
nnoremap <leader>nf :NERDTreeFind<cr>
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

