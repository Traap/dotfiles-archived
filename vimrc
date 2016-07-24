" Author: gary.a.howard@mac.com
"
 "Influenced by:
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

let config_list = ['plugin-mgr-vim-plug.vim', 'settings.vim']

for files in config_list
  for f in split(glob(s:vim_home.files), '\n')
    exec 'source '.f
  endfor
endfor
