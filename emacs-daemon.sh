#!/bin/bash

EMACS="`which emacs`"

# This isn't working due to the emacsclient side on the *.app
#if [ "`uname`" = "Darwin" ]
#then
#    EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
#fi

$EMACS --daemon

