#!/bin/sh

[ -f "$HOME/.xprofile" ] && . $HOME/.xprofile

exec i3 > /tmp/i3.log 2>&1
