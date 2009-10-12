;;;-*- auto-recompile: t -*-
;; Time-stamp: <2001-01-24 12:16:34 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Package: elder
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 3.4.1release
;; 

;;; elder.el; ELDER, the ELisp Document writER/formattER/unitER. 
 (setq elder-version "3.4.1release")
;;; UNITES LISP WITH OTHER LANGUAGES.
;;; See comments at the end.

;;;Copyright (C) Deepak Goel
;;;Author(s)
;;;---Deepak Goel deego@glue.umd.edu
;;; UNRESTRICTIONS: see README 

;;; Commentary: COMPLETE ELDER DISTRIBUTION AVAILABLE AT
;;; http://www/glue.umd.edu/~deego/elder/
;;; Contains tons of examples, and style-files..


;;; ELDER is known to work with emacs20.3.1.  I haven't tried it on
;;; any other emacs.  But i think it should work on all platforms and
;;; most recent emacsen.

;;;Althoguh the end-user can set global-variables like
;;;*elder-aliases*, *elder-delimiter-list* by hand, it is recommended
;;;that you use elder-provided functions to manipulate these
;;;variables, like (elder-alias), (elder-unalias) etc.
;;;
;;;
;;; If you work with files that require heavy elder-processing, please
;;; do not forget to increase the values of max-lisp-eval-depth and
;;; max-specpdl-size's to large numbers.. for instance, 30000 and
;;; 12000..


;;;Fri Jan 19 13:38:54 2001
;;;###autoload
(defvar elder-loading-hook nil "run before loading elder.." )
(run-hooks 'elder-loading-hook)

(require 'cl)
(if (not noninteractive)
    (ignore-errors (require 'elder-set-keys)))
;;;THESE DEFALIAS'es are maintained for historic reasons, AND WILL SLOWLY
;;;BE PHASED OUT. PLEASE TRY TO USE THE ACTUAL FUNCTIONS IN ANY OF
;;;YOUR PROGRAMS..
(defalias 'etex 'elder-etex)
(defalias 'emat 'elder-emat)
(defalias 'protect 'elder-protect)
(defalias 'estyle 'elder-estyle)
(defalias 'ealias 'elder-alias)
(defalias 'ealiasp 'elder-alias-p)
(defalias 'ealiasbind 'elder-alias-bind)
(defalias 'eunalias 'elder-unalias)
(defalias 'eregionalias 'elder-alias-region)
(defalias 'ealias-list 'elder-alias-list)

(defvar elder-buffers-to-retain '(nil nil nil)
  "Whether to retain the initbuf, workbuf and finalbuf respectively,
after elder-general'ing a file.. 
Currently, only the finalbuf's choices are accepted.. Others are
killed anyway!! ")

;;;====================================================
(defvar elder-status-interval 40
"The more the interval, the less the messages.."
)
(defvar elder-protection-string
      "[[[ELDERPROTECTIONSTRING]]]"
      "This is used by a function elder-protect."
      )

(defmacro elder-bind (a &optional default)
  "If a not bound, bind it to default. copied from .emaxcs.macros"
  (list 'if (list 'not (list 'boundp (list 'quote a)))
	(list 'setq a default))
  )
(defmacro elder-bindpair  (a b &optional default)
  "Assumes b is opposite of a. If either one if bound, binds other.
If none is boundp, then a is assigned default, and b its opposite.
Copied from emacs.macros.."
  (list 'if (list 'boundp (list 'quote a))
	(list 'setq b (list 'not a))	
	(list 'if (list 'boundp (list 'quote b))
	      (list 'setq a (list 'not b))
	      (list 'progn
		    (list 'setq a default))
	      (list 'setq b (list 'not a))))
  )



(defun elder-protect-new (string)
  "This results in protecting the alias in the string from being
expanded. The string should please be > 1 letter long. DOES NOT
PROTECT ALIASES OF 1 CHARACTER LONG FROM BEING EXPANDED.
Prefer this over eregionalias.
Example, ([ebf]protect bal[eef]) won't expand the bal, even though bal
may have been aliased to \\begin{align}. It works by inserting
elder-proection-string after the first character of the sting to be
protecting, and right at the end-of-buffer, ealiasing the new string
back to the old string. 
You might be tempted to try to just insert 
\(ealias elder-protection-string \"\" \) at the end-of-buffer, but see
that this approach will lead to failure if 2 or more strings are
protected. 
elder-protection-string not only protects the current string from
being expanded, but also all substrings of the current string.
"
  (let ((newstring (elder-protect-find-new-string string)))
    (insert newstring)
    (goto-char (point-max))
    (insert 
     "([ebeg] (eunalias "
          (elder-stringize string)
	  ") (elder-alias "
	  (elder-stringize newstring) 
	 (elder-stringize string) " ) [eend])")
    "")
)

(defun elder-protect (string)
  "This results in protecting the alias in the string from being
expanded. The string should please be > 1 letter long. DOES NOT
PROTECT ALIASES OF 1 CHARACTER LONG FROM BEING EXPANDED.
Prefer this over eregionalias.
Example, ([ebf]protect bal[eef]) won't expand the bal, even though bal
may have been aliased to \\begin{align}. It works by inserting
elder-proection-string after the first character of the sting to be
protecting, and right at the end-of-buffer, ealiasing the new string
back to the old string. 
You might be tempted to try to just insert 
\(ealias elder-protection-string \"\" \) at the end-of-buffer, but see
that this approach will lead to failure if 2 or more strings are
protected. 
elder-protection-string not only protects the current string from
being expanded, but also all substrings of the current string.
"
  (let ((newstring (elder-protect-find-new-string string)))
    (goto-char (point-max))
    (insert 
     "([ebeg] (eunalias "
          (elder-stringize string)
	  ") (elder-alias "
	  (elder-stringize newstring) 
	 (elder-stringize string) " ) [eend])")
    newstring)
)

(defun elder-protect-find-new-string (string)
  "INTERNAL. Finds a temporary new string that will thus protect the current
string from being expanded. " 
  (apply 'concat
	 (mapcar 
	  (lambda (char)
	    (concat (char-to-string char) elder-protection-string))
	  (string-to-list string)))
)




;;;====================================================

(defmacro elder-detailed-error (arg)
  "Wrap this around a code to generate detailed error-message if any.
The code is executed normally, except that in case any error occurs,
the error-message is more detailed. The emacs's error is indicated,
along with the current location of the elder-document being
processed.
TO DO: save workbuffer upon occurence of error."

  (list 'condition-case 'err arg
	(list 'error (list 'error
			   (list 'concat 
				 "THIS ERROR OCCURED:\n"
				 (list 'error-message-string 'err)
				 "\nWHILE WE ARE HERE:"
				 (list 'elder-stringize-string
				       (list 'elder-show-position
					     (list 'point))))))))


;;;====================================================

(require 'cl)
(setq elder-trace nil)

(defun elder-parse-csh-list (string)
 "Given a string  as a csh-list, makes it a lisp-list. Basically
copied from .emacs.macros on 9/23/00.." 
 (let 
     ((ll (length string)))
   (if (<= ll 0)
       nil
     (with-temp-buffer
       (insert string)
       (let ((firstpos
	      (progn
		(goto-char (point-min))
		(search-forward ":" nil t))))
	 (if (null firstpos)
	     (list string)
	   (cons (buffer-substring (point-min) (- firstpos 1))
		 (elder-parse-csh-list
		  (buffer-substring firstpos
				    (point-max)))))))))
)

(defun elder-getenv (var)
  "Gets the variable as a list of strings rather than single string.
You get the idea.. The : is perhaps always assumed to be a delimiter.. even
when preceded by a // . Might someday get to look into
this.. Basically copied from .emacs.macros on 9/23/00."

  (elder-parse-csh-list (getenv var))
)

(defun elder-insert-file-contents (file &optional path)
"Finds file from within elder-path and inserts it here. 
After help from  comp.emacs.help, realize that could have simply used
the emacs-provided function locate-library.
As a result: this is what happens:
First this looks for file.est [, .el, .elc] from your load-path, and
if that is not found, this looks for file[,.el,.elc] from your
load-path..

"
  (if (null path)
      (setq path load-path))
  (let ((aa (locate-library (concat file ".est")))
	(bb (locate-library file)))
    (if aa
	(elder-insert-stripped-file-contents aa)
      (if bb
	  (elder-insert-stripped-file-contents bb)
	(error
	 (concat "ELDER Error: File to be inserted " file " not found")
	 (elder-show-position (point))))))
)



;;; (defun elder-insert-file-contents-old (file path)
;;; "Finds file from within elder-path and inserts it here. 
;;; After help from  comp.emacs.help, realize that could have simply used
;;; the emacs-provided function locate-library."
;;;   (if 
;;;       (null path)
;;;       (progn
;;; 	(error  
;;; 	 (concat "ELDER Error: File to be inserted " file " not found")
;;; 	 (elder-show-position (point))))
;;;     (let ((this-file (concat (car path) "/" file)))
;;;       (if (file-exists-p this-file)
;;; 	  (elder-insert-stripped-file-contents this-file)
;;; 	(elder-insert-file-contents file (cdr path)))))
;;; )


(defun elder-insert-stripped-file-contents (file)
  "Strips all occurences of comments in the file and inserts it.
The file, stripped of its comments is inserted at the current
\(point\)."
  (let 
      ((content 
	(with-temp-buffer
	  (insert-file-contents-literally file)
	  (if
	      elder-commentstripp
	      (elder-strip-comments (buffer-name) elder-comment))
	  (buffer-substring (point-min) (point-max)))))
    (insert content))
)





(defun elder-estyle (&rest args)
  "Adds .est to each filename, and inserts them at current position. 
Returns a string \"\". 
Actually, from now on, since elder-insert-file-contents is being
modified, the file added will be the one from load-path which matches
the one found by elder-insert-file-contents.."
  (mapcar 
   (lambda (arg)
     ;;; AM i supposed to use insert-file-contents-literally here???
     (elder-insert-file-contents (concat arg ".est") ))
   args)
  ""
)


(defvar elder-comment "")
(defvar elder-noticep nil)
(defvar elder-commentstripp nil)
(defvar elder-elder-commentstripp nil)
(defvar elder-begin "([ebeg]")
(defvar elder-end "[eend])")
(defvar elder-roughbuffer "*elder-roughbuffer*")
(defvar elder-aliases '() )
(defvar elder-elbf "([ebf]")
(defvar elder-elef "[eef])")
(defvar elder-elff "[eff]")
(defvar elder-path nil)

(defun elder-message-shell (&rest args)
  (shell-command (apply 'concat "echo " 
			(mapcar 
			 (lambda (arg)			    
			   (format "%S" (concat 
					 (if (stringp arg)
					     arg
					   (format "%S" arg)) " ")))
			 args))))


(defun elder-message (&rest args)
  (apply 'message args)
  )

(defun elder-space-p ()
  "Tells if the following character is a whitespace."
  (member (following-char) '(9 10 32)))

(defvar elder-aliases '()
  "List of alias-defs. 
Each alias-def is of the form '(alias expansion). Alias is string. And
expansion is an expression whch is evaluated at the time of alias-expansion."
)
;;;###autoload
(defun elder-defaults ()
"Basic defaults for elder.
Is meant to be internal to elder, but maybe has some interactive use too?"
  (interactive)

;   probably not needed any more since have made elder iterative now-->
;  (if (< max-lisp-eval-depth 10000)
;      (setq max-lisp-eval-depth 10000)) ;;;increase it even more for
;				       ;;;longer documents. 
;  (if (< max-specpdl-size 20000) 
;      (setq max-specpdl-size 20000))
  (setq elder-comment "")
  (setq elder-noticep nil)
  (setq elder-commentstripp nil)
  (setq elder-elder-commentstripp nil)
  (setq elder-begin "([ebeg]")
  (setq elder-end "[eend])")
  (setq elder-roughbuffer "*elder-roughbuffer*")
  (setq elder-aliases '() )
  (setq elder-elbf "([ebf]")
  (setq elder-elef "[eef])")
  (setq elder-elff "[eff]")
  (setq elder-path nil)
  (mapcar (lambda (element)
	    (add-to-list 'elder-path element))
	  (reverse (elder-getenv "ELDERPATH")))
  ;; first, elder-path if any, then load-path..
  (setq load-path (append elder-path load-path))
  (add-to-list 'load-path ".")
  (defvar elder-after-setting-load-path-hook nil)
  (run-hooks 'elder-after-setting-load-path-hook)

  (elder-alias "[bck]"
	'(progn
	   (delete-backward-char 1)
	   "")
	"[del]"
	'(progn
	   (delete-backward-char -1) 
	   "")
	)
  (message "Elder-defaults set..")
)

;;;====================================================

(defun elder-alias (&rest args)
"Is meant to be internal to elder, but maybe has some interactive use too?"

  (elder-aliaslist-internal args))


(defun elder-aliaslist-internal (args)
"internal to elder."

  (unless (null args) 
    (progn
      (let ((aa (elder-replace-already-defined (list (car args) (cadr args))
					 elder-aliases)))
	(if aa (setq elder-aliases aa)
	  (setq elder-aliases
		(cons (list (car args) (cadr args)) elder-aliases))))
      (elder-aliaslist-internal (cddr args))))
)

; added 11/23/00 to v.2.6:
(defun elder-alias-p (arg)
  "Tells you if the arg is aliased to something"
  (interactive "sString:") ; doesn't work great yet.
  (eval (cons 'or
	 (mapcar (lambda (analias) (equal arg (car analias)))
		 elder-aliases)))
  )

(defun elder-alias-bind (aa bb)
  "If aa is not already aliases to something, then alias it to bb."
  (if (not (elder-alias-p aa))
      (elder-alias aa bb)))

(defun elder-alias-list (a a-list some-quoted-symbol)
  " Rotates a's expansion from a-list.
Aliases a such that its replacements are chosen in sequence from
the quoted list.  The SOME-QUOTED-SYMBOL keeps track of which element
of the list is being chosen.  Very useful, for instance if you want
succeeding quotes to be replaced alternatively by open-quote and
close-quote. 
 Suggestion: If/when u like, You may change the value of your
some-quoted-symbol in the middle of the program to overrule linear
rotation..
"
 (set some-quoted-symbol 0)
 (elder-alias a
	 `(progn
	    (let* ((a-list (quote ,a-list))
		  (this-string (eval (nth ,some-quoted-symbol a-list))))
	      (setq ,some-quoted-symbol 
		   (% (+ ,some-quoted-symbol 1)
		      (length a-list)))
	      (if (not (stringp this-string))
		  (error "Ealias-sequence did not return string!"))
	      this-string)))
)

		  
						       



;;;====================================================

(defun elder-unalias (&rest args)
  "Undefines all the arguments.
Is meant to be internal to elder, but maybe has some interactive use too?"

  (elder-unalias-internal args))

(defun elder-unalias-internal (args)
  (if (null args) nil
    (progn
      (setq elder-aliases
	    (apply 'append
		    (mapcar 
		     (lambda (element)
		       (if (string= (car args) (car element))
			   nil (list element)))
		     elder-aliases)))
      (elder-unalias-internal (cdr args)))))



;;;====================================================
(defun elder-replace-already-defined (newalias aliaslist)
"If already defined, returns a substituted aliases, else gives nil"
 (if (null aliaslist) nil
   (if (string= (car newalias) (caar aliaslist))
       (cons newalias (cdr aliaslist))
     (let ((testothers (elder-replace-already-defined
			newalias (cdr aliaslist))))
       (if (null testothers) nil
	 (cons (car aliaslist) testothers)))))
)


;;;====================================================
(defun elder-strip-comments (workbuffer olcomment)
  "The strip-comments routine..
Compare to elder-strip-comments-all though.."
  (set-buffer workbuffer)
  (let* ((comment (concat "
" olcomment))
	 (cl (length comment))
	 (eldone nil)
	 (curpos nil))
    (while (not eldone)
      (progn
	(goto-char (point-min))
	(setq curpos (search-forward comment nil t))
	(if (null curpos)
	    (setq eldone t)
	  (progn 
	    (setq curpos (- curpos cl))
	    (goto-char curpos)
	    (kill-line)
	    (kill-line)
	    ))))))

;;;====================================================
(defun elder-strip-comments-all (workbuffer comment)
  "Strips ALL occurences of comments..
As opposed to elder-strip-comments which strips only the lines that
start with a comment. This one strips everything until the end-of-line
after it sees a comment, even if the comment doesnot start at the
beginning of a line.."
 (set-buffer workbuffer) 
 (goto-char (point-min))
 (let ((cl (length comment)) (eldone nil) (curpos nil))
   (if (> cl 0)
       (while (not eldone)
	 (progn
	   (goto-char (point-min))
	   (setq curpos (search-forward comment nil t))
	   (if (null curpos)
	       (setq eldone t)
	     (progn
	       (setq curpos (- curpos cl))
	       (goto-char curpos)
	       (if (string= (buffer-substring (- curpos 1) curpos)
			    "
")
		   (progn (kill-line) (kill-line))
		 (kill-line))


	       ))))))
)
;;;====================================================

(defun elder-setnthcar (n ls expr)
  (if (> n 0) (cons (car ls) (elder-setnthcar (- n 1) (cdr ls) expr))
    (setcar ls expr) ls)
)

;;;====================================================

;;;###autoload
(defun elder-etex (initbuf)
  "Elder's Latex-specific invocation.
Note that this function expects filenames ending in .e.


If you call this function interactively, and give it a filename not
ending in .e, it will result in an error.

If you call this in batch-mode, and give it an <argument> not ending in
.e, it will assume that you meant <argument>.tex.e.

The function takes the filename ending in .tex.e, invokes elder on it,
and stores the resulting file as file.tex.

"

  (interactive "FFile:  ")
  (elder-defaults)
  (let* ((buffers (elder-bufnames initbuf ".tex.e"))
	 (finalbuf (second buffers)))
    (setq elder-comment "%")
    (setq elder-noticep t)
    (setq elder-commentstripp t)
    (elder-general (car buffers) finalbuf)
))

;;;###autoload
(defun elder-emat (initbuf)
  "Elder's matlab-specific invocation.
Note that this function expects filenames ending in .e.


If you call this function interactively, and give it a filename not
ending in .e, it will result in an error.

If you call this in batch-mode, and give it an <argument> not ending in
.e, it will assume that you meant <argument>.m.e.

The function takes the filename ending in .m.e, invokes elder on it,
and stores the resulting file as file.m.
"

  (interactive "FFile:  ")
  (elder-defaults)
  (let* ((buffers (elder-bufnames initbuf ".m.e"))
	(finalbuf (second buffers)))
    (setq elder-comment "%")
    (setq elder-noticep t)
    (setq elder-commentstripp nil)
    (elder-general (car buffers) finalbuf)
))

;;;###autoload
(defun elder-ehtml (initbuf)
  "Elder's html-specific invocation.
Note that this function expects filenames ending in .e.


If you call this function interactively, and give it a filename not
ending in .e, it will result in an error.

If you call this in batch-mode, and give it an <argument> not ending in
.e, it will assume that you meant <argument>.m.e.

The function takes the filename ending in .m.e, invokes elder on it,
and stores the resulting file as file.m.
"

  (interactive "FFile:  ")
  (save-excursion
    (elder-defaults)
    (let* ((buffers (elder-bufnames initbuf ".html.e"))
	   (finalbuf (second buffers)))
      (setq elder-comment "")
      (setq elder-noticep nil)
      (setq elder-commentstripp nil)
      (elder-general (car buffers) finalbuf)
      )))

;;;###autoload
(defun elder (initbuf)
 "ELDER for a general language. Use elder-etex etc. for specific languages.
Use elder-etex for Latex, elder-emat for Matlab etc."
 (interactive "FFile:")
 (save-excursion
   (elder-defaults)
   (let* ((buffers (elder-bufnames initbuf ".e"))
	  (finalbuf (second buffers)))
     (setq elder-comment "")
     (setq elder-noticep nil)
     (setq elder-commentstripp nil)
     (elder-general (car buffers) finalbuf)
   )))
;;;====================================================
(defun elder-bufnames (initbuf ext)
"Internal to ELDER, sorts out proper buffer-names. 
Assumes that the argument ext ends in .e ."
  (if 
      (and
       (> (length initbuf) (length ext))
       (string= ext
		(substring initbuf (- (length initbuf) (length ext))
			   (length initbuf))))
      (list initbuf (substring initbuf 0 (- (length initbuf) 2)))
    (let ((newinit (concat initbuf ext)))
      (list newinit
	    (substring newinit 0 (- (length newinit) 2)))))
 )

;;;====================================================

;(defmacro elder-detailed-error (arg)
;  arg)


;;;===============================================================

(defun elder-general (initbuf finalbuf)

  "Not recommended for end-user. Works on any language.  
End-user: Use elder instead
"
;;;(let ((elder-tmp-buffer (current-buffer)))
  (let ((initial-buffer (buffer-name)))
    (save-excursion
      (let* ((workbuffer (concat initbuf ".log"))
	     (initfile
	      (if noninteractive (concat default-directory initbuf)
		initbuf))
	     (workfile
	      (if noninteractive (concat default-directory workbuffer)
		workbuffer))
	     (finalfile 
	      (if noninteractive (concat default-directory finalbuf)
		finalbuf)))
	;; kill any previous instances of files
	;; (ignore-errors (kill-buffer initfile))
	(ignore-errors (kill-buffer workfile))
	(ignore-errors (kill-buffer finalfile))
	(find-file workfile)
	(set-visited-file-name nil)
	(rename-buffer workfile nil)
	(set-buffer workfile)
	(setq case-fold-search nil)
	(kill-region (point-min) (point-max))
;;;     (elder-debug-indicate-stats)  ;;;ERR
	
;;;     (find-file initfile)
;;;     (kill-buffer initbuf)
	(if (not (file-exists-p initfile))
	    (error 
	     (concat "ELDER Error: Can't find your initial file " initfile
		     ))
	  )
;;;     (rename-buffer initfile nil)
;;;     (if (file-exists-p finalfile)
;;;	 (delete-file finalfile))
;;;     (copy-region-as-kill (point-min) (point-max)) ; of initfile..
	(set-buffer workfile)
	(auto-fill-mode -1) ; turns it off
	(insert "                                                    ")
	(insert "
")
	(insert-file-contents  initfile)
	(insert "                                                    ")
	(insert "
")
    ;;;these inserts of spaces help my search-es return no errors.
	
	(if elder-commentstripp
	    (elder-strip-comments workfile elder-comment))
	(write-file 
	 workfile nil)
	(rename-buffer workfile)
	(set-visited-file-name nil)
	(let (s-ctr)
	  (setq s-ctr (elder-buffer workfile))
	  
	  (goto-char (point-min))
	  (if elder-noticep
	      (progn
		(insert elder-comment)
		(insert "DO NOT EDIT THIS FILE! Your changes will be")
		(insert " overwritten next\n")
		(insert elder-comment)
		(insert "time this file is automatically generated from")
		(insert " its source-file \n")
		(insert elder-comment)
		(insert initbuf)
		(insert " via ELDER v. ")
		(insert elder-version)
		(insert ". Please go edit that file instead\n")
		(insert elder-comment)
		(insert "This File: ")
		(insert finalfile)
		(insert (format " after %S substitutions." s-ctr))
		(insert " \n")
		)))
	(write-file 
	 workfile nil)
	(rename-buffer workfile nil)
	(set-visited-file-name nil)
	(write-file 
	 finalfile  nil)
	(rename-buffer finalfile nil)
	(set-visited-file-name nil)
	(kill-buffer finalfile)
	)
      )
    (switch-to-buffer initial-buffer))
  )


;;;====================================================
(defun elder-buffer (&optional buffer max-times)
  "Returns the number of substitutions made.. will be used by elder.. 
BUFFER is the name of the buffer to be elderred..
Stops any further processing after max-times..
"
  (interactive)
  (if (null buffer) (setq buffer (buffer-name)))
  (set-buffer buffer)
  (if elder-interval (apply 'sit-for elder-interval))
  (if elder-wait-for-user-p
      (let ((continue nil))
	(while (not continue)
	  (setq continue 
		(y-or-n-p "Press y to continue..")))
	(message nil)))
  
  (let ((s-ctr 0))
    (let ((processedp t))
      (while (and processedp (or (null max-times) (>= max-times s-ctr)))
	(incf s-ctr)
	(if (zerop (% s-ctr elder-status-interval))
	    (elder-message "Running elder-substitute: %S" s-ctr))
	(setq processedp (elder-substitute buffer))
	(if elder-interval (apply 'sit-for elder-interval))
	(if elder-wait-for-user-p
	    (let ((continue nil))
	      (while (not continue)
		(setq continue 
		      (y-or-n-p "Press y to continue..")))
	      (message nil)))
	))
    s-ctr)
  )

;;;====================================================
(defun elder-buffer-interactively (buffer)
  "Waits for some time after each change.. "
  (interactive "b buffer: ")
  (let ((elder-interval 1))
    (elder-buffer buffer)))

;;;====================================================
(defvar elder-interval nil
  "If nil, no wait..
Else elder sit-for's for these many seconds after every substitution
for you to see..  This should be a list of the 2 arguments to be
passed to sit-for..

")

(defvar elder-wait-for-user-p nil
  "If nil, no wait..
Else elder sit-for's for these many seconds after every substitution
for you to see.. ")



;;;====================================================

(defun elder-stringize-string (s)
  "Turns all single % to double.. so that format can handle it.."
  (interactive "s string:  " )
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (replace-match "%%" nil t))
    (buffer-substring (point-min) (point-max)))
)


(defun elder-stringize (arg)
  "INTERNAL. Easy: Stringizes the argument. Copied from .emacs.xxx"
 (format "%S" arg))
;;;====================================================

(defun elder-findfirstelbf (workbuffer)
  (set-buffer workbuffer)
  (goto-char (point-min))
  (list
   (search-forward elder-elbf nil t)
   (list elder-elbf ""))
)
			       

;;;====================================================
(defun elder-findfirstelderbegin (workbuffer)
  (set-buffer workbuffer)
  (goto-char (point-min))
  (list
   (search-forward elder-begin nil t) 
   (list elder-begin "")))
;;;====================================================


(defun elder-findfirstalias (workbuffer)
  "Returns the first elder-alias, found, 
along with the 2 matching aliases. 
Is meant to be internal, but could it be possibly useful interactively?" 

  (set-buffer workbuffer)
  (let* ((aliaslist (elder-findaliaslist workbuffer elder-aliases))
	 (bestyet '(nil ("" ""))))
    (while (not (null aliaslist))
      (if (not (null (caar aliaslist)))
	  (if (or (null (car bestyet)) (> (car bestyet) (caar aliaslist)))
	      (setq bestyet (car aliaslist))))
      (setq aliaslist (cdr aliaslist)))
    bestyet)
)
;;;====================================================
(defun elder-findaliaslist (workbuffer aliases)
  (if (null aliases) nil
    (cons 
     (list
      (progn
	(goto-char (point-min))
	(search-forward (caar aliases) nil t))
      (car aliases))
     (elder-findaliaslist workbuffer (cdr aliases))))
)


;;;====================================================

;;;Fri Jan 19 14:16:16 2001
;;;###autoload
(defun elder-substitute-interactive (n)
  "Will work on the current buffer.."
  (interactive "p")
  (let ((name (buffer-name)))
    (dotimes (tmp-var n)
      (elder-substitute name)))
)



;;;###autoload
(defun elder-substitute (&optional workfile)
  "Internal to ELDER. Does one substitution. WORKFILE is a buffer, not file.
Is meant to be internal, but could it be possibly useful
interactively?
" 
  (interactive )
  (if (null workfile) (setq workfile (buffer-name)))
  (set-buffer workfile) 
  (let ((change-made nil)		
	(firstalias (elder-findfirstalias workfile))
	(firstelbegin (elder-findfirstelderbegin workfile))
	(firstelbf (elder-findfirstelbf workfile))
	firstfound cutregionend cutregionbegin)
  
    (unless (and (null (car firstalias)) (null (car firstelbegin))
		 (null (car firstelbf)))

;;;====================================================
      (setq change-made t)
      (setq firstfound
	    (if (not (null (car firstalias)))
		(if (or (null (car firstelbegin)) 
			(< (car firstalias) (car firstelbegin)))
		    (if (or (null (car firstelbf))
			    (< (car firstalias) (car firstelbf)))
			"alias"
		      "elbf")
		  (if (or (null (car firstelbf))
			  (< (car firstelbegin) (car firstelbf)))
		      "elbegin"
		    "elbf" ))
	      (if (null (car firstelbegin)) 
		  "elbf"
		(if (or (null (car firstelbf))
			(< (car firstelbegin) (car firstelbf)))
		    "elbegin" "elbf" ))))
      (if (string= firstfound "elbegin")
	  (progn 
;;;	    (elder-message-shell "\nProcessing " (elder-buffer-substring
;;;					 (car firstelbegin)
;;;					 (+ (car firstelbegin) 10)))
	    (goto-char (point-min))
	    (setq cutregionend
		  (elder-search-matching-end 
		   elder-begin 1 elder-end 0
		   (search-forward elder-begin nil t)))
	    (if (null cutregionend)
		(error (concat "\nELDER Error: Missing " elder-end
			       " for the " elder-begin " found at "
			       "point " 
			       (format "%S" (car firstelbegin))
			       "-----\n"
			       "You might have missed some other"
"[eef], not necessarily this one.\n"
   "Look for dangling [eef]s with no ) \n"
			       "-----\n"
			       (elder-show-position (car firstelbegin))
			     )))
	    (setq cutregionbegin (- (car firstelbegin) (length
							elder-begin)))



	    (goto-char cutregionbegin)
	    (let ((expressions (buffer-substring 
				(car firstelbegin)
				(- cutregionend (length elder-end))))
		  tobeevaluated)
	      (kill-region cutregionbegin cutregionend)
	      (elder-detailed-error
	       (with-temp-buffer
		 (insert "(setq tobeevaluated (quote (progn " 
			 expressions " )))")
		 (eval-buffer)))
	      (elder-detailed-error 
	       (let ((aa (eval tobeevaluated)))
		 (goto-char cutregionbegin)
		 (if (stringp aa)
		     (insert aa)
		   (if (numberp aa)
		       (insert (format "%S" aa)))))))

;          the stuff before i modified it:
; 	    (goto-char cutregionend)
; 	    (insert (elder-execute workfile (car firstelbegin) 
; 				(- cutregionend (length elder-end))
; 				*elder-roughbuffer*))
; 	    (kill-region cutregionbegin cutregionend)

	    ))

      (if (string= firstfound "alias")
	  (progn
;	    (elder-message-shell "\nProcessing " (elder-buffer-substring
; 					 (car firstalias)
; 					 (+ (car firstalias) 10)))
	    (setq cutregionend (car firstalias))
	    (setq cutregionbegin (- cutregionend 
				    (length (caadr firstalias))))
	    (let ((aliasname
		   (buffer-substring cutregionbegin cutregionend)))
	      (kill-region cutregionbegin cutregionend)
	      (goto-char cutregionbegin)

	      (elder-detailed-error (insert 
			       (eval (cadadr firstalias))))
; 	        (condition-case err (eval (cadadr firstalias))
;		  (error (error
;			  (error-message-string err)
;			  "while evaluating alias"
;			  (elder-show-position (point))))
	      )
	))
      (if (string= firstfound "elbf") 
	  (progn
;	    (elder-message-shell "\nProcessing " (elder-buffer-substring
;					 (car firstelbf)
;					 (+ (car firstelbf) 10)))
	  (goto-char (point-min))
	  (goto-char (search-forward elder-elbf nil t))
	  (setq cutregionend (elder-search-matching-end elder-elbf 1 
						     elder-elef 0 (point)))

	  (if (null cutregionend)
	      (error (concat "\nELDER Error: Missing " elder-elef " for the "
			     elder-elbf " at point " 
			     (format "%S" (car firstelbf)) 
			     "-----\n"
			     "If not, some other [eef]) is missing.\n"
			     "Look for dangling [eef]s with no ) \n"
			     "-----\n"
			     (elder-show-position (car firstelbf))
			     )))
	  (setq cutregionbegin (- (car firstelbf) (length
						  elder-elef)))
	  (goto-char (+ cutregionbegin (length elder-elbf))) 
	  (insert " "); now cutregion has increased by 1...
	  (setq cutregionend (+ 1 cutregionend))
	  (goto-char (+ cutregionbegin (length elder-elbf))) 
	  (forward-sexp 1)
	  (let ((function-string 
		 (buffer-substring 
		  (+ cutregionbegin (length elder-elbf)) (point)))
		(remainder-string 
		 (progn
		   (while (elder-space-p)
		     (forward-char))
		   (if (string= 
			elder-elff
			(buffer-substring 
			 (point)
			 (+ (point) (length elder-elff))))
		       (goto-char (+ (point) (length elder-elff))))
		   (buffer-substring (point) 
				     (- cutregionend 
					(length elder-elef))))))
	    (goto-char cutregionend )
	    (insert " ")
	    (insert elder-begin)
	    (insert " ")
	    (insert "(apply")
	    (insert (concat "(quote " function-string ") "))
	    (insert "(quote (")
	    (insert (format "%S" remainder-string))
	    (insert " ))")
	    (insert ")")
	    (insert elder-end)
	    (kill-region cutregionbegin (+ cutregionend 1)) )))

      (if elder-trace 
	  (progn
	    (write-file 
	     workfile nil)
	    (set-visited-file-name nil)
	    (rename-buffer workfile)))

;      (set-buffer "tmpfile")
;      (goto-char (point-max))
;      (insert "\naliases:\n\n")
;      (goto-char (point-max))
;      (insert (format "%S" *elder-aliases*))
;      (goto-char (point-max))
;      (insert "\n\nworkfile:\n")
;      (goto-char (point-max))
;      (insert-buffer workfile)
;      (goto-char (point-max))
;      (insert "\n")
;      (set-buffer workfile)
;       the above is a debugging tool..				       

)

      change-made
      )) 
;;;elder-substitute ENDS HERE..

;;;====================================================
; Not needed as of elder21.
; (defun elder-execute (workbuffer startregion endregion roughbuffer)
;   (set-buffer workbuffer)
;   (let ((expressions (buffer-substring startregion endregion))
; 	(elder-tmp-result "") tmpstring)
;     (get-buffer-create roughbuffer)
;     (set-buffer roughbuffer)
;     (setq tmpstring (buffer-substring (point-min) (point-max)))
;     (kill-region (point-min) (point-max))
;     (goto-char (point-min))
;     (insert " (setq elder-tmp-result (progn ")
;     (insert expressions)
;     (insert " )) ")
;     (eval-buffer roughbuffer)
;     (kill-region (point-min) (point-max))
;     (goto-char (point-min))
;     (insert tmpstring)
;     (set-buffer workbuffer)
;     (if (integerp elder-tmp-result) 
; 	(setq elder-tmp-result (format "%S" elder-tmp-result)))
;     (if (numberp elder-tmp-result)
; 	(setq elder-tmp-result (format "%d" elder-tmp-result)))
;     (if (not (stringp elder-tmp-result))
; 	(setq elder-tmp-result ""))
;    (set-buffer workbuffer)
;     elder-tmp-result )
; )


;;;====================================================
(defun elder-show-position (position)
 "Returns a string indicating buffer-contents near position.
Very useful as a error-position-indicator.
IS meant to be internal to elder. But maybe has some use as interactive?"
  (interactive) ; doesn't work great yet.
  (concat "\n"
   (elder-buffer-substring (- position 60)
			(- position 0))
   "\n<--- ELDER WAS AT THIS POINT (Error) *********\n"
   (elder-buffer-substring (+ position 0)
			(+ position 60)) "\n" )
)

;;;====================================================
(defun elder-search-matching-end (beginstring bnumber endstring enumber startpt)
  "Tries to search for matching end.
Tries to mimic search-forward in that returns nil if not found."
  (if (<= bnumber enumber) 
      startpt
    (let ((nextbegin
	   (progn
	     (goto-char startpt)
	     (search-forward beginstring nil t)))
	  (nextend
	   (progn
	     (goto-char startpt)
	     (search-forward endstring nil t))))
      (if (null nextend) nil
	(if (or (null nextbegin) (> nextbegin nextend))
	    (elder-search-matching-end beginstring bnumber endstring 
				    (+ enumber 1) nextend)
	  (elder-search-matching-end beginstring (+ bnumber 1) endstring 
				  enumber nextbegin))))))


;;;====================================================
(defun elder-buffer-substring (a b)
  (buffer-substring
   (if (< a (point-min)) (point-min) a)
   (if (> b (point-max)) (point-max) b)))

;;;====================================================
(defun elder-search-matching-begin (beginstring bnumber endstring enumber startpt)
  "Tries to search for matching end.
Tries to mimic search-backward in that returns nil if not found.
Provided just for completeness though elder doesn't use it."
  (if (<= enumber bnumber) 
      startpt
    (let ((nextbegin
	   (progn
	     (goto-char startpt)
	     (search-backward beginstring nil t)))
	  (nextend
	   (progn
	     (goto-char startpt)
	     (search-backward endstring nil t))))
      (if (null nextbegin) nil
	(if (or (null nextend) (> nextbegin nextend))
	    (elder-search-matching-begin beginstring (+ 1 bnumber) endstring 
				    enumber nextbegin)
	  (elder-search-matching-begin beginstring bnumber endstring 
				  (+ 1 enumber) nextend))))))




;;;====================================================

(defun elder-generate-random-string (length)
  (if (zerop length)
      ""
    (concat "*&*"
     (format "%c" (+ 97 (random* 26)))
        ; we use random* since that seems to be truly random..
     (elder-generate-random-string (- length 1)))))

;;;====================================================



(defun elder-alias-region (string1 string2 pos1 pos2)
  "Optional extension. Allows aliasing within the 2 positions.
Achieves the effect that aliasing takes place only in certain regions
of the document.  Is not a necessary part of ELDER. Just uses ELDER to
build on it.  pos2 and pos1 are unique strings the user places
somewhere in the buffer. pos2 should occur *after* pos1.  If u decide
to use this function, please do not have any other aliases or keywords
named *&* or substrings thereof. If you do, there is a 1 in 26^(length
of yr alias) chance that yr alias might conflict with the random alias
being used by elder-alias-region.

DON'T USE IF YOU CAN HELP IT. IS NOT EFFICIENT OR ELEGANT EITHER. IS
KINDA EXTERNAL TO ELDER. TRY protect FOR A MUCH SIMPLER
IMPLEMENTATION.. unfo., this protect does not yet exist as of 10/4/00.

"
  (let ((string3 (elder-generate-random-string 10)) 
	(pos3 (elder-generate-random-string 10)))
    (elder-alias string1 string3)
    (elder-alias pos2
	    (list 'progn
		  (list 'eunalias string1)
		  (list 'elder-alias string3 string1)
		  (list 'elder-alias pos1
			(list 'quote 
			      (list 'progn
				    (list 'eunalias string3)
				    "")))
		 pos3))
    (elder-alias pos3 
	    (list 'progn
		  (list 'elder-alias string3 string2)
		  ""))
    )
)
    

(defun elder-require ()
  "Loads elder.."
  (interactive)
  (require 'elder)
)



;;;Mon Jan 15 19:45:32 2001
;;;###autoload
(defun elder-debug-message (&rest args)
  (apply 'message args)
  (sit-for 2)
)


;;;Mon Jan 15 19:22:23 2001
(defun elder-debug-indicate-stats ()
  (error
   "size: %S Name: %S" (buffer-size) (buffer-name)))


;;;====================================================
;;; next, for the elder-game..
;;;====================================================
(defvar elder-life-string 
  "








   *      *  (==    *    (==    ==)   *  *  (==   (===    * *   





")

;;;###autoload
(defvar elder-life-buffer "*elder-life*")

;;;###autoload
(defun elder-life ()
  "Elder's own mini 1-D game of life..
Will reinitialize any game in progress.."
  (interactive)
  (elder-defaults)
  (setq elder-aliases nil)
  (elder-life-aliases)
  (get-buffer-create elder-life-buffer)
  (switch-to-buffer elder-life-buffer)
  (widen)
  (kill-region (point-max) (point-max))
  (insert elder-life-string)
  (elder-life-play '(0 200))
  )

(defalias 'elder-life-aliases  'elder-life-aliases-2)

(defun elder-life-aliases-2 ()
  (elder-alias " * "     " = "
	       " = "     '(elder-random " ==" "== ")
	       " == "     '(elder-random "(==" "==)")
	       "==) " " ==)"
	       " (==" "(== "
	       "\n(==" "\n== "
	       "\n== " "\n=  "
	       "\n= " "\n  "
	       "==)\n" " ==\n"
	       " ==\n" "  =\n"
	       " =\n" "  \n"
	       "==)==)" '(elder-random "==)   "
				       "==)(==")
	       "(==(==" '(elder-random "==)(==" "   (==")
	       " ==)(== " '(elder-random
			    "==)*(== " " ==)*(==" )
	       "==)*" "(==*"
	       "*(==" "*==)"
	       )
)  
(defun elder-life-aliases-1 ()
  (elder-alias " * "     " = "
	       " = "     '(elder-random " ==" "== ")
	       " == "     '(elder-random " ===" "=== ")
	       "===" '(elder-random "(==" "==)")
	       " ==)  " "  ==) "
	       "  (== " " (==  "
	       "\n(==" "\n== "
	       "\n== " "\n=  "
	       "\n= " "\n  "
	       "==)\n" " ==\n"
	       " ==\n" "  =\n"
	       " =\n" "  \n"
	       "==)==)" "==)   "
	       "(==(==" "   (=="
	       "==) (==" '(elder-random "(== *==)" "(==* ==)")
	       "==)(==" "(====)"
	       "==)*(==" "(==*==)"
	       
	       )
)  

(defun elder-life-play (&optional interval)
  (let ((elder-interval interval))
    (elder-buffer elder-life-buffer))
  (message "Done.. To play again, press C-x spc spc e l ")
)


(defun elder-random (&rest args)
  (nth (random (length args)) args)
)




;;;Fri Jan 19 13:38:35 2001
;;;###autoload
(defvar elder-loaded-hook nil
  "After loading elder..")
;;;Fri Jan 19 13:38:43 2001
(run-hooks 'elder-loaded-hook)

;;;Fri Jan 19 15:46:33 2001
(defun elder-set-min (sym val)
  (if (< (eval sym) val)
      (set sym val)))

;;;Tue Jan 23 18:12:03 2001
;;;###autoload
(defun elder-ehtml-this-file ()
  "Saves and ehtml's this file.. "
  (interactive)
  (elder-save-and-elder 'elder-ehtml))
;;;###autoload
(defun elder-etex-this-file ()
  "Saves and etex's this file.. "
  (interactive)
  (elder-save-and-elder 'elder-etex))
;;;###autoload
(defun elder-emat-this-file ()
  "Saves and emat's this file.. "
  (interactive)
  (elder-save-and-elder 'elder-emat))


;;;Tue Jan 23 18:14:42 2001
;;;###autoload
(defun elder-save-and-elder (function)
  "meant to be Internal to elder..
Saves the current buffer, and processes the corresponding file.."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
	(progn
	  (save-buffer)
	  (apply function file nil))
      (error "This does not look like a file.."))))

(defvar elder-tutorial-buffer "*elder-INTRO*")

(defalias 'elder-tutorial 'elder-example)
(defalias 'elder-introduction 'elder-example)
(defalias 'elder-example 'elder-example-step-0)

(defun elder-example-step-0 ()
  ""
  (interactive)
  (get-buffer-create elder-tutorial-buffer)
  (switch-to-buffer elder-tutorial-buffer)
  (delete-other-windows)
  (kill-region (point-min) (point-max))
  (insert "This is an introduction to elder.."
	  "ELDER allows you to mix lisp with any document.."
	  "ELDER processes all the lisp-regions it finds in the file..")
  (insert
   "Of course, each lisp-region can lead to creation of more
lisp-regions, whic again are processed by elder.. In each iteration,
elder processed the first lisp-region it finds in the file..")
  (call-interactively 'fill-paragraph)
  (insert "\n\n")
  
  (insert "YOU WILL NOW WATCH step-by-step HOW ELDER PROCESSES A FILE..")

  (call-interactively 'fill-paragraph)
  (insert "\n\n")

  (insert "There are 3 types of lisp-regions: [a] plain simple lisp region..
       [b] function-region [c] alias-region ")
  (call-interactively 'fill-paragraph)
  (insert "\n\n")
  (insert "HERE'S SOME MORE INFO: [a] is simply a region containing lisp, usually demarcated
   by ([EBEG] and [EEND]) (in small case..), all the lisp will be
   processed and the last expression's value will be substituted..")
  (call-interactively 'fill-paragraph)
  (insert "\n\n")
  (insert "[a] is sufficient for all purposes, but [b] and [c] are
   lisp constructs used so commonly that they get their own
     regions.. ")
  (insert "[b] is an elder-function-region, which begins with ([EBF]
    followed by a lisp-fucnction name followed by some white-space. The
    function will be applied to the string gathered after this
    function, ending on [EEF]), and the result will be
    substituted.. There's some more provisions in this., see documentation.."
	  )
  (insert "[c] is a region the text in which matches elder-alias.. In
   which case, you guessed it, the alias is expanded.. eithe rto a
  plain string, or a result of any eval on the expression you have
  defined the alias to be..  There's tons of associated
  provisions.. see documentation.." )
  (call-interactively 'fill-paragraph)
  (insert "\n\n")
  (insert "Now for the tutorial.. Press C-d to continue..")
  (goto-char (point-min))
  (message "Read all. Then, Press C-d to see live action :)")
  (local-set-key "\C-d" 'elder-example-step-1)
)
  
(defvar elder-example-file "example.elder")

(defun elder-example-step-1 ()
  "Stage 1.. Try different files for more examples.."
  (interactive)
  (let ((elder-wait-for-user-p t))
    (get-buffer-create elder-tutorial-buffer)
    (switch-to-buffer elder-tutorial-buffer)
    (kill-region (point-min) (point-max))
    (if (locate-library elder-example-file)
	(elder-insert-file-contents elder-example-file)
      (progn
	(message "Can't find example file.  Will generate own..")
	(sit-for 1)
	(elder-insert-default-example)))
    (goto-char (point-min))
    (elder-buffer elder-tutorial-buffer))
  )


(defun elder-example-html ()
  "an example of html-processing..
Have you ever had to type links twice while editing html?
Once to link, and once for them to show up (if u want to).. ? Then,
use see how this file manuals.html.e avoids that... and see its
processing.."


  (interactive)
  (let
      ((elder-example-file 
	"manuals.html.e"))
    (elder-example-step-1)))


(defun elder-insert-default-example ()
  "Generates example file.. and tries to put it in the current
directory.."
  (insert
   "
this file is a simple example of elder.. Watch how ELDER will compute
the following for you:

====================================================
3^6 = ([ebeg] \"some junk here, just for fun\"
              \"more junk..\"
       (expt 3 6)  [eend])
====================================================

or perform a reversal on some string....


====================================================
([ebeg]
 (defun reverse-my (text)
   (if (string= text \"\")
       \"\"
     (concat (reverse-my (substring text 1 (length text)))
	     (substring text 0 1))))
 [eend])

====================================================

The reverse of pallindrome is ([ebf]reverse-my pallindrome[eef]).

====================================================

ELDER has some default aliases: for example -[bck], which
backspaces... (such aliases would be defined if you call M-x
elder-defaults).  plus you can define your own.. or remove the
default, or whatever..
====================================================
ELDER has modes..  M-x elder-etex  has some latex-specific defaults.., as
does elder-ehtml and elder-emat..   NO, elder does not do
regexp-aliasing (yet.. help welcome!)

If you use elder a lot: You can group your usually used
elder-constructs into elder-style-files..  I have, for instance, my
own html.est, and article.est, and fundamental.est..  elder-style
files can be inserted, (yeah, you are right, you could have also 
achieved this using your own insert.. )...    


To see a simple example, go ahead and type M-x  elder-example-html
"
))

(provide 'elder)

;;;elder.el ends here..

