" utilities.vim

" Quick editing
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>eb :vsplit ~/git/dotfiles/vim/base.vim<cr>
nnoremap <leader>ed :vsplit ~/git/dotfiles/vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>ef :vsplit ~/git/dotfiles/vim/functions.vim<cr>
nnoremap <leader>ek :vsplit ~/git/dotfiles/vim/keymappings.vim<cr>
nnoremap <leader>el :vsplit ~/git/dotfiles/vim/leader.vim<cr>
nnoremap <leader>es :vsplit ~/git/dotfiles/vim/settings.vim<cr>
nnoremap <leader>eu :vsplit ~/git/dotfiles/vim/utilities.vim<cr>
nnoremap <leader>e. :vsplit ~/git/dotfiles/bash_profile<cr>
nnoremap <leader>ea :vsplit ~/git/dotfiles/alias_and_functions<cr>
nnoremap <leader>eg :vsplit ~/git/dotfiles/gitconfig<cr>
nnoremap <leader>eS :vsplit ~/git/dotfiles/setup.hs<cr>
nnoremap <leader>eB :vsplit ~/git/dotfiles/vim/getbundles.hs<cr>

"NERD Tree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
