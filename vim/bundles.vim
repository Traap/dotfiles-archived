" bundles.vim {{{ "
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
vnoremap  <expr>  <LEFT>   DVB_Drag('left')
vnoremap  <expr>  <RIGHT>  DVB_Drag('right')
vnoremap  <expr>  <DOWN>   DVB_Drag('down')
vnoremap  <expr>  <UP>     DVB_Drag('up')
vnoremap  <expr>  D        DVB_Duplicate()
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
nnoremap <silent><leader>N  :NERDTreeToggle<CR>
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
" 1et g:LatexBox_latexmk_async = 0
" let g:LatexBox_quickfix = 2
" let g:LatexBox_split_length = 15
" -------------------------------------------------------------------------- }}}
" CTRL-P {{{
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }

nnoremap <silent> <leader>z :CtrlP<CR>
nnoremap <silent> <leader>Z :CtrlPBuffer<CR>
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
