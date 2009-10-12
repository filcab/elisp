;;;-*- auto-recompile: nil -*-

 (defvar article-loading-hook nil)
 (run-hooks 'article-loading-hook)
 (setq article-version "3.2.0.release")
 ""

(defvar article-document "article")
;; could be anything, like revtex4, foil etc.
;; (elder-bindpair foilp articlep nil) 	

(if (string-match "article" article-document)
    (progn
      (defvar articlep t)
      (defvar foilp nil)
      (defvar revtexp nil))
  (if (string-match "revtex" article-document)
      (progn
	(defvar articlep t)
	(defvar revtexp t)
	(defvar foilp nil))
    (progn
      (defvar articlep nil)
      (defvar revtexp nil)
      (defvar foilp nil))))

(defun articlenonrevtexonly (text)
  (if (and articlep (not revtexp))
      text ""))


(defun foilonly (string)
   (if foilp string ""))

(defun articleonly (string)
  (if articlep string ""))
 
 (elder-bind article-yesfoil-string "\\yesfoil")
 (elder-bind article-nofoil-string "\\nofoil")
 ""


;;; (goto-char (point-max))
;;; (unless (string-match "article-lisp" (buffer-name))
;;;   (elder-insert-file-contents "articleend"))


 (defvar article-max-specpdl-size 30000)
 (defvar article-max-lisp-eval-depth 20000)
 (elder-set-min 'max-specpdl-size article-max-specpdl-size)
 (elder-set-min 'max-lisp-eval-depth article-max-lisp-eval-depth)
 (elder-bind elder-trace nil)


 


 (elder-bind  *formal-version* nil)  ; True means: final, though not complying
					; with APS idiosyncracies like
			       ; monochrome, 12pt.
 (elder-bind  *manuscript-version* nil)   ; True means: very formal, send for
			       ; manuscript, and thus has
			       ; inconveniences like 12pt, monochrome.



 (elder-bindpair *print-version* *view-version* t)
			 ;The document we are producing is henceforth
			 ;is a *print-version*, not a *view-version*,
			 ;since we now know how to tweak gv to view
			 ;small fonts..
			       ;OLD ADVICE: 
			       ;Please always keep this nil, since
			       ;this file automatically generates an
			       ;additional print<file>.tex where this
			       ;*print-version* is effectively t.
				     ; True means: 
					;suited for printing .. 
					;==> font may be too small for gv-ing

   ; Usually, my e-texing a document shall auto-generate a printmct.tex
   ; --this file will be fit for printing while mct.tex will be for
   ; for gv-ing..


					; (setq article-comment-strip nil)
  (if *manuscript-version* (setq *formal-version t))
                           ; but of course..
 ""
      



 (if *formal-version* (elder-bind article-comment-strip t)
   (elder-bind article-comment-strip nil))
 (elder-bind article-comment-strip2 t)
 (elder-bind article-comment-strip3 t)
 ; there are thus 3 types of comments.. use cc, cc2 and cc3 and each
 ; can be turned on/off separately. 

; (if *manuscript-version* 
;     (elder-bind *print-option* "monochrome")
;   (elder-bind *print-option* "dvipsnames"))
 (if *manuscript-version*
     (elder-bind *color-option* nil)
   (elder-bind *color-option* t))
  (if *color-option*
      (elder-bind *color-device-option* "dvips,dvipsnames")
    (elder-bind *color-device-option* "monochrome"))

;;COLORING SCHEME: The top of the colorstack shall always contain the
;;current color. 

; Use the following information (and functions if needed) for colors.
; Note that use of the functions tmplistize will slow things down.
;    (progn
; (setq tmpnamedcolors '(
; "{GreenYellow}"   "{cmyk}{0.15,0,0.69,0}"
; "{Yellow}"        "{cmyk}{0,0,1,0}"
; "{Goldenrod}"     "{cmyk}{0,0.10,0.84,0}"
; "{Dandelion}"     "{cmyk}{0,0.29,0.84,0}"
; "{Apricot}"       "{cmyk}{0,0.32,0.52,0}"
; "{Peach}"         "{cmyk}{0,0.50,0.70,0}"
; "{Melon}"         "{cmyk}{0,0.46,0.50,0}"
; "{YellowOrange}"  "{cmyk}{0,0.42,1,0}"
; "{Orange}"        "{cmyk}{0,0.61,0.87,0}"
; "{BurntOrange}"   "{cmyk}{0,0.51,1,0}"
; "{Bittersweet}"   "{cmyk}{0,0.75,1,0.24}"
; "{RedOrange}"     "{cmyk}{0,0.77,0.87,0}"
; "{Mahogany}"      "{cmyk}{0,0.85,0.87,0.35}"
; "{Maroon}"        "{cmyk}{0,0.87,0.68,0.32}"
; "{BrickRed}"      "{cmyk}{0,0.89,0.94,0.28}"
; "{Red}"           "{cmyk}{0,1,1,0}"
; "{OrangeRed}"     "{cmyk}{0,1,0.50,0}"
; "{RubineRed}"     "{cmyk}{0,1,0.13,0}"
; "{WildStrawberry}""{cmyk}{0,0.96,0.39,0}"
; "{Salmon}"        "{cmyk}{0,0.53,0.38,0}"
; "{CarnationPink}" "{cmyk}{0,0.63,0,0}"
; "{Magenta}"       "{cmyk}{0,1,0,0}"
; "{VioletRed}"     "{cmyk}{0,0.81,0,0}"
; "{Rhodamine}"     "{cmyk}{0,0.82,0,0}"
; "{Mulberry}"      "{cmyk}{0.34,0.90,0,0.02}"
; "{RedViolet}"     "{cmyk}{0.07,0.90,0,0.34}"
; "{Fuchsia}"       "{cmyk}{0.47,0.91,0,0.08}"
; "{Lavender}"      "{cmyk}{0,0.48,0,0}"
; "{Thistle}"       "{cmyk}{0.12,0.59,0,0}"
; "{Orchid}"        "{cmyk}{0.32,0.64,0,0}"
; "{DarkOrchid}"    "{cmyk}{0.40,0.80,0.20,0}"
; "{Purple}"        "{cmyk}{0.45,0.86,0,0}"
; "{Plum}"          "{cmyk}{0.50,1,0,0}"
; "{Violet}"        "{cmyk}{0.79,0.88,0,0}"
; "{RoyalPurple}"   "{cmyk}{0.75,0.90,0,0}"
; "{BlueViolet}"    "{cmyk}{0.86,0.91,0,0.04}"
; "{Periwinkle}"    "{cmyk}{0.57,0.55,0,0}"
; "{CadetBlue}"     "{cmyk}{0.62,0.57,0.23,0}"
; "{CornflowerBlue}""{cmyk}{0.65,0.13,0,0}"
; "{MidnightBlue}"  "{cmyk}{0.98,0.13,0,0.43}"
; "{NavyBlue}"      "{cmyk}{0.94,0.54,0,0}"
; "{RoyalBlue}"     "{cmyk}{1,0.50,0,0}"
; "{Blue}"          "{cmyk}{1,1,0,0}"
; "{Cerulean}"      "{cmyk}{0.94,0.11,0,0}"
; "{Cyan}"          "{cmyk}{1,0,0,0}"
; "{ProcessBlue}"   "{cmyk}{0.96,0,0,0}"
; "{SkyBlue}"       "{cmyk}{0.62,0,0.12,0}"
; "{Turquoise}"     "{cmyk}{0.85,0,0.20,0}"
; "{TealBlue}"      "{cmyk}{0.86,0,0.34,0.02}"
; "{Aquamarine}"    "{cmyk}{0.82,0,0.30,0}"
; "{BlueGreen}"     "{cmyk}{0.85,0,0.33,0}"
; "{Emerald}"       "{cmyk}{1,0,0.50,0}"
; "{JungleGreen}"   "{cmyk}{0.99,0,0.52,0}"
; "{SeaGreen}"      "{cmyk}{0.69,0,0.50,0}"
; "{Green}"         "{cmyk}{1,0,1,0}"
; "{ForestGreen}"   "{cmyk}{0.91,0,0.88,0.12}"
; "{PineGreen}"     "{cmyk}{0.92,0,0.59,0.25}"
; "{LimeGreen}"     "{cmyk}{0.50,0,1,0}"
; "{YellowGreen}"   "{cmyk}{0.44,0,0.74,0}"
; "{SpringGreen}"   "{cmyk}{0.26,0,0.76,0}"
; "{OliveGreen}"    "{cmyk}{0.64,0,0.95,0.40}"
; "{RawSienna}"     "{cmyk}{0,0.72,1,0.45}"
; "{Sepia}"         "{cmyk}{0,0.83,1,0.70}"
; "{Brown}"         "{cmyk}{0,0.81,1,0.60}"
; "{Tan}"           "{cmyk}{0.14,0.42,0.56,0}"
; "{Gray}"          "{cmyk}{0,0,0,0.50}"
; "{Black}"         "{cmyk}{0,0,0,1}"
; "{White}"         "{cmyk}{0,0,0,0}"
; ))

; (defun tmplistize (list)
;   "Groups arguments by lists of 2, so that assoc works.."
;   (if (null list) nil
;     (cons (list (car list) (cdr list)) (tmplistize (cddr list)))))

; (setq namedcolors (tmplistize tmpnamedcolors))

; (ealias "\\makedefaultcolor" "\\makeatletter
; \\let\\default@color\\current@color \\makeatother ")

;  (ealias "\\makedefaultcolor" " ")


; colorlist needs to be defined even if not using colors. So that
; \\color{blue} can be aliases to ""
  (elder-bind colorlist
	  '( ("mathcolor" "{blue}")
	     ("footmarkcolor" "{red}")
	     ("textcolor" "{black}")
	     ("tagcolor"    "[rgb]{0,0.6,0}")
	     ("commentcolor" "[rgb]{0,0.3,0}")
	     ("logocolor" "{yellow}")
	      
;             ("commentcolor" "[cmyk]{0.99,0,0.52,0}") 
					; JungleGreen from
					;	      dvipsnam.def
	     ("citecolor" "[rgb]{.9,.2,.25}") ; purple?
					;	      ("citecolor" "[rgb]{.8,.3,.3}") ; brown?
					;	      ("citecolor" "[cmyk]{0.12,0.59,0,0}") ; Thistle
	     ("flagcolor" "{red}")))
 
;; colorstack stores colors -- that part of color that follows after
;; the "\\color" part.



;; NOTE: If color-option is nil, then (cadr colorstack) will give
;; nothing. Plus, in that case, we alias \\color to "".

; \\restorecolor pops from the color-stack and sets that color.
; \\refreshcolor does the same thing, but still leaves it on the
; colorstack.

  (if *color-option*
     (ealias "\\restorecolor" 
		      '(if t
			   (let 
			       ((finalstring 
				 (concat "\\color" (cadr colorstack) )))
			     (setq colorstack (cdr colorstack))
			     (concat finalstring
				     "\\resetdefaultcolor")
			     )))
    (ealias "\\restorecolor" ""))

  (if *color-option*
     (ealias "\\refreshcolor" 
		      '(if t
			   (let 
			       ((finalstring 
				 (concat "\\color" (car colorstack) )))
			     (concat finalstring
				     "\\resetdefaultcolor")
			     )))
    (ealias "\\refreshcolor" ""))


     
;      (defun definecolors (colorlist)
;        "Is perhaps not needed at all, the way i am working...
;Moreover, if defined, might interfere with stuff. For, \\citecolor
;will match both as an command as well as an alias (the latter half
;citecolor would be an alias. This we don't want this.."
;        (if (null colorlist) ""
; 	 (progn
; 	   (with-temp-buffer
; 	     (insert "(setq ")
; 	     (insert (caar colorlist))
; 	     (insert (format "%S" (cadar colorlist)))
; 	     (insert ")" )
; 	     (eval-buffer))
; 	   (definecolors (cdr colorlist)))))
;      (definecolors colorlist)

     (defun definecolorcommands (colorlist)
       "Should probably run definecolors before this."
       (if (null colorlist) ""
	 (progn
	   (with-temp-buffer
	     (insert "(ealias \"\\\\"
			 (caar colorlist) "\" ")

	     (if *color-option*
		 (insert
		  "(quote 
                         (progn
			  (setq colorstack 
			       (cons "
			        (format "%S" (cadar colorlist))
				" colorstack )) "
				"( concat \"\\\\color\" "
			       (format "%S" (cadar colorlist)) 
			       "\"\\\\resetdefaultcolor \"" ")))"
			     )
	       (insert "\"\"" ))
	     (insert ")")
	     (eval-buffer))
	   (definecolorcommands (cdr colorlist)))))


     (definecolorcommands colorlist)
                           
                                                             
(if *color-option*
   (setq colorstack (list (cadr (assoc "textcolor" colorlist))))
  (progn
    (setq colorstack nil)
    (ealias "\\color" "")
    )
   )

 (ealias "\\." "\\cdot")
 (ealias "\\\\<" "\\left<")
 (ealias "\\\\>" "\\right>")
 (ealias "\\\\(" "\\left(")
 (ealias "\\\\)" "\\right)")
 (ealias "\\\\[" "\\left[")
 (ealias "\\\\]" "\\right]")
 (ealias "\\\\{" "\\left\\{")
 (ealias "\\\\}" "\\right\\}")
 (ealias "[[]" "\\mathcolor")   ;for blue, in general
 (ealias "[]]" "\\restorecolor")
 (ealias "(()" "\\footmarkcolor"); for red, in general
 (ealias "())" "\\restorecolor")
 (ealias "\\begin{al}" (concat " \\mathcolor" "\\begin{align}") 
	"\\end{al}" (concat "\\end{align}" "\\restorecolor"))
 (ealias "\\begin{eq}" (concat "\\mathcolor" "\\begin{equation}") 
	     "\\end{eq}" (concat "\\end{equation}" " \\restorecolor"))
 (ealias "\\\\vol<" "\\left\\langle" 		
	     "\\\\vol>" "\\right\\rangle_{\\mbox{vol}}")
 (ealias "\\begin{eqarr}" (concat "\\mathcolor" "\\begin{eqnarray}")
	     "\\end{eqarr}" 
	     (concat "\\end{eqnarray}" "\\restorecolor"))
 (ealias "[[$" (concat "\\mathcolor" "$"))

 ; The space after $ is important, I think..			$
 (ealias "$]]" '(concat "$" 
			(if (member (following-char) '(9 10 32)) " " "")
			"\\restorecolor"

			;;Mon Jan 22 21:34:19 2001 do not wanna
			;;introduce space if the line seems to end in
			;;a , or . or ;
			(if (member (following-char) '(44 46 59)) "" " ")
			))
 ""








 (if *formal-version* 
	(elder-bind *article-no-tags* t)
	(elder-bind *article-no-tags* nil)
	)
 (defun ref (string)
   (concat "\\mathcolor" "\\eqref{" string
 	   "} " "\\restorecolor"))
 (defun cite (string)
   (concat "\\citecolor" "\\cite{" string 
	   "}" "\\restorecolor"))
 
; Please note that haven't used \\footmarkcolor, thus we are NOT
; pushing footmarkcolro onto the colorstack...
 (defun foot (string)
   (concat 
    (if *color-option* "\\color" "")
    (if *color-option* (cadr (assoc "footmarkcolor" colorlist)) "")
    "\\footnote{" 
    "\\resetdefaultcolor"
    string " }" "\\color" (car colorstack)))
 
 (ealias "\\eref{"
	  '(progn
	     (insert "([ebf]ref ")
	     (goto-char (search-forward "}" nil t))
	     (delete-backward-char 1)
	     (insert "[eef])")
	     ""))

 (ealias "\\ecite{"
	 '(progn
	    (insert "([ebf]cite ")
	    (goto-char (search-forward "}" nil t))
	    (delete-backward-char 1)
	    (insert "[eef])")
	    ""))
 

 (defun ll (string)
   "Will have to strip the first space since the function definitions
currently return the first space as well"
   (let ((string string))
     (if *article-no-tags*
	 (concat "\\label{" string "}")
       (concat "\\label{" string "} " 
	       "\\thickspace \\thickspace" 
	       "\\rotatebox{0}{$\\underset{\\mbox{\\tiny{"
	       "\\tagcolor" (upcase string) "\\restorecolor" "}}}{   }$}"
	       "\\thickspace \\thickspace" 
	       )
     )))

 (defun ccc (string)
   (if article-comment-strip  
       "" string)
   )
 (defun cc (string)
   "see usrguide. "
   (if article-comment-strip ""
       (concat "\\commentcolor" "[[" string "]]" "\\restorecolor")))

 (defun cc2 (string)
   (if article-comment-strip2 ""
       (concat "\\commentcolor" "[2[" string "]2]" "\\restorecolor")))
 (defun cc3 (string)
   (if article-comment-strip3 ""
       (concat "\\commentcolor" "[3[" string "]3]" "\\restorecolor")))

 (defun ccflag (string)
   (if article-comment-strip ""
 ;      (concat "\\flagcolor" "{\\bf " string "}" "\\restorecolor")
	(concat "([ebf]cc " "\\flagcolor" string "\\restorecolor"
		"[eef])")
	))

 (defun flag (string)
   (concat "\\flagcolor" "{\\bf " string "}" "\\restorecolor"))
""




   

  (defun print-my (&rest args)
   (shell-command (apply 'concat "echo " 
			 (mapcar 
			  (lambda (arg)			    
			    (format "%S" (concat 
					  (if (stringp arg)
					      arg
					    (format "%S" arg)) " ")))
			  args))))
 


(defun current-time-my ()
  "Tells you the current time. Copied mostly from emacs.macros.."
  (let ((aa (decode-time)))
    (concat 
     (format "%S" (third aa))
     ":"
     (two-zeros-my (format "%S" (second aa)))
     ":"
     (two-zeros-my (format "%S" (first aa)))))
)


(defun two-zeros-my (string)
  "Gives minutes in a mm format.
If the input string is one letter long, append a zero before it. 
Useful  for the function current-time-my (). Copied mostly from
.emacs.macros
 "
 (if (= (length string) 1)
     (concat "0" string)
   string))





 (if foilp
     (progn
       (ealias "\\subsection" "\\foilhead")
       (ealias "\\section" "\\foilhead")))





(ealias article-yesfoil-string "")

(ealias article-nofoil-string
	'(let ((thispoint (point))
	       (nextyes (search-forward article-yesfoil-string nil t)))
	   (if foilp 
	       (let ((cutregionend 
		      (if (null nextyes)
			  (point-max)
			(- nextyes (length article-yesfoil-string)))))
		 (kill-region thispoint cutregionend)))
	   "")
	)

; (defun processyesnofoil (string)
;   "If foil-mode, then process the \\nofoil and \\yesfoil commands.  If
; foilp is nil, ignore the \\nofoil and \\yesfoil commands.  If foilp is
; t, means the document is a foil. A command \\nofoil within the
; argument of processyesnofoil means, start stripping text from
; hereafter unless you see a \\yesfoil.. Default is \\nofoil.  In
; non-foils-mode (means foilp = nil), stuff may be included from the
; article using the function foilonly.. Also consider function
; articleonly.. WILL NOW BE OBSOLETE SINCE AM ALIASING \\YESFOIL AND
; \\NOFOIL PROPERLY.. "
;     (pynf-internal  "\\nofoil" string)
; )

; (defun pynf-internal (mode string)
;   (with-temp-buffer
;     (insert string)
;     (let*
; 	((nextno
; 	  (list "\\nofoil" 
; 		(progn
; 		  (goto-char (point-min))
; 		  (search-forward "\\nofoil" nil t))))
; 	 (nextyes 
; 	  (list "\\yesfoil"
; 		(progn
; 		  (goto-char (point-min))
; 		  (search-forward "\\yesfoil" nil t))))
; 	 (nextremove 
; 	  (select-lesser-internal nextyes nextno)))
;       (if (null (second nextremove))
; 	  (if (string= mode "\\nofoil") 
; 	      (if foilp "" string)
; 	    string)
; 	(progn
; 	  (concat
; 	   (if (and (string= "\\nofoil" mode) foilp)
; 	       ""
; 	     (buffer-substring (point-min) 
; 			       (+ 0 (- (second nextremove)
; 				       (length (first nextremove))))))
; 	   (pynf-internal
; 	    (first nextremove)
; 	    (buffer-substring (second nextremove) (point-max))))))))
;   )
	    
; (defun pynf-internal-old (position mode)
;  (let*
;      ((nextno 
;        (list "\\nofoil" (progn
; 	 (goto-char position)
; 	 (search-forward "\\nofoil" nil t))))
;       (nextyes
;        (list "\\yesfoil"
; 	     (progn
; 	       (goto-char position)
; 	       (search-forward "\\yesfoil" nil t))))
;       (nextremove
;        (select-lesser-internal
; 	nextyes nextno)))
;    (if (null (second nextremove))
; 	 (if (string= mode "\\nofoil")
; 	     (kill-region position (point-max)))
;      (progn
;        (if (string= mode "\\nofoil")
; 	   (kill-region position (second nextremove))
; 	 (kill-region (- (second nextremove) (length "\\yesfoil"))
; 		      (second nextremove)))
;        (if (string= (first nextremove) "\\yesfoil")
; 	   (pynf-internal (second nextremove) "\\yesfoil")
; 	 (pynf-internal (second nextremove) "\\nofoil"))))))

(defun select-lesser-internal (a b)
  (if (null (second b))
      a
    (if (null (second a))
	b
      (if (< (second a) (second b))
	  a b)))) 

;  (defun processyesnofoilold (string)
;    "does not seem to work..Please do not use this if you do not
; understand this. Please use functions foilonly and articlesonly
; instead. This one behaves differently. This is meant to generally be
; used on the entire following document just once. Will remove
; everything after any \"\\nofoil\\\" it shall encounter, unless it
; comes across a \"\\yesfoil\\\". Please do not use these keywords
; anywhere from now on, even in lisp programming unless you really mean
; to... "
;    (with-temp-buffer
;      (insert "\\nofoil ")
;      (insert string)
;      (goto-char (point-min))
;      (let (nextno nextyes)
;        (while
; 	   (setq nextno 
; 		 (progn
; 		   (goto-char (point-min))
; 		   (search-forward "\\nofoil" nil t)
; 		   ))
; 	 (setq nextyes
; 	       (if foilp
; 		   (progn
; 		     (goto-char nextno)
; 		     (search-forward "\\yesfoil" nil t)
; 		     )
; 		 nextno))
	       
; 	 (if (null nextyes)
; 	     (setq nextyes (point-max)))
; 	 (kill-region (- nextno (length "\\nofoil")) nextyes)
; 	 ))
;      (goto-char (point-min))
;      (let (nextremove)
;        (while
; 	   (setq nextremove
; 		 (progn
; 		   (goto-char (point-min))
; 		   (search-forward "\\yesfoil" nil t)
; 		   ))
; 	 (goto-char nextremove)
; 	 (kill-region (- nextremove (length "\\yesfoil"))
; 		      nextremove)))
;      (buffer-substring (point-min) (point-max))
;      )
;  )
""



(ealias "\\resetdefaultcolor" "") 



 (defun eabstract (string)
   (concat (if *formal-version* ""
	     "([ebf]ccflag ([ebeg] (current-time-my)
               [eend]). This printout belongs to Deepak.[eef])")
	   string))
 (defun html (string)
     (concat "\\htmladdnormallink{" string "}{" string "}")
	  )
""




(elder-bind fig-ctr 0) "" 



 (elder-bind *header* "articleheader")
 (unless (string-match "article-lisp" (buffer-name))
   (elder-insert-file-contents *header*))
""







 (defvar article-loaded-hook nil)
 (run-hooks 'article-loaded-hook)
 ""

