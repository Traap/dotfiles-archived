#!/bin/bash
#----------------------------------------------------------------------
# Don't do anything when not running interactively.
#----------------------------------------------------------------------
[ -z "$PS1" ] && return

#----------------------------------------------------------------------
# Source my dotfiles personalized environment.
#----------------------------------------------------------------------
if [ -f ~/git/dotfiles/alias_and_functions ]; then
  source ~/git/dotfiles/alias_and_functions
fi

# added by travis gem
[ -f /Users/gary/.travis/travis.sh ] && source /Users/gary/.travis/travis.sh
