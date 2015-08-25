" Acknowledgement
" .vimrc
" Author: gary.a.howard@mac.com
" Source: https://github.com/Traap/dotfiles
"
" Influenced by:
"    Steve Losh <steve@stevelosh.com>
"      http://bitbucket.org/sjl/dotfiles/src/tip/vim/
"    
"    James Morrin
"      https://github.com/treasonx/
"    
"    Tim Pope
"      https://github.com/tpope/
"
let s:vim_home = '~/git/dotfiles/vim/'

let config_list = [ 
  \ 'base.vim',
  \ 'settings.vim', 
  \ 'bundles.v:wim', 
  \ 'leader.vim',  
  \ 'keymappings.vim'
\]

for files in config_list 
  for f in split(glob(s:vim_home.files), '\n')
    exec 'source '.f
  endfor
endfor
