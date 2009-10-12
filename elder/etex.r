#!/usr/bin/tcsh

echo Don''t worry about the first error that might just \
follow--">";\
 /bin/rm elder-invoke.el;\
 echo \(load \"\$ELDER/elder.elc\"\) >elder-invoke.el;\
 echo \(etex \"$1\"\) >>elder-invoke.el;\
 $EMACS -batch -l $DOTEMACSELDER -l elder-invoke.el;\
 /bin/mv elder-invoke.el elder-invoke.el~


### PLEASE SET UP THESE VARIABLES: EMACS AND DOTEMACSELDER 
### IF YOU PREFER RUNNING ELDER IN THE BATCH MODE, THE VARIABLE
### DOTEMACSELDER should contain an init file you want emacs to load..
    
