#!/bin/bash

EMACS = `which emacs`

if [ `uname` = "Darwin" ]
then
    EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
fi

$EMACS --daemon
