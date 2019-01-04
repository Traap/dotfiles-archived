mklink    .bash_logout        c:\Users\${USERNAME}\git\dotfiles\bash_logout
mklink    .bash_profile       c:\Users\${USERNAME}\git\dotfiles\bash_profile   
mklink    .bashrc             c:\Users\${USERNAME}\git\dotfiles\bashrc 
mklink    .config.vim         c:\Users\${USERNAME}\git\ssh\config.vim 
mklink    .dircolors          c:\Users\${USERNAME}\git\dotfiles\dircolors 
mklink    .gitconfig          c:\Users\${USERNAME}\git\ssh\gitconfig
mklink    .gitignore_global   c:\Users\${USERNAME}\git\ssh\gitignore_global 
mklink    .inputrc            c:\Users\${USERNAME}\git\dotfiles\inputrc 
mklink    .minttyrc           c:\Users\${USERNAME}\git\dotfiles\minttyrc 
mklink    .mutt               c:\Users\${USERNAME}\git\mutt-office365 
mklink    .muttrc             c:\Users\${USERNAME}\git\mutt-office365\muttrc 
mklink    .tmux.conf          c:\Users\${USERNAME}\git\tmux\tmux.conf 
mklink /D .vim                c:\Users\${USERNAME}\git\vim
mklink    .viminfo            c:\Users\${USERNAME}\git\vim\viminfo
mklink    .vimrc              c:\Users\${USERNAME}\git\vim\vimrc
mklink    .vimrc_background   c:\Users\${USERNAME}\git\vim\vimrc_background
mklink /D .ssh                c:\Users\${USERNAME}\git\ssh 
mklink /D .tmux               c:\Users\${USERNAME}\git\tmux 

echo off
rem # I do this to force my ssh dir to my window home directory.  I'm tired of
rem # fighting corporate networks, vpn, and other stuff. Note: I may not have
rem # privileges to create a link on the hdrive.
rem
rem # mklink /D h:\.ssh c:\Users\${USERNAME}\git\ssh
