;;; latexize.el --- 
;;;----------------------------------------------------------------------
;; Author: Joao Cachopo <jcachopo@gia.ist.utl.pt>
;; Created on: Fri Nov 16 14:30:02 2001
;; Keywords: 
;;
;; Copyright (C) 2001 Joao Cachopo

;; This program is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Massachusettes Ave, Cambridge, MA
;; 02139, USA.

;;; Commentary:
;;

;;; Code:

(require 'htmlize)

(defvar latexize-code-faces nil
  "List of faces used by latexize.")

(defvar latexize-frame nil
  "Frame used to font-lockify the code, such that we may have a custom background/foreground.")

(defvar latexize-code-background nil
  "Background color used for the code.")

(defvar latexize-code-foreground nil
  "Foreground color used for the code.")

(defvar latexize-latex-command-char ?¡)
(defvar latexize-latex-start-group-char ?«)
(defvar latexize-latex-end-group-char ?»)

;; (defvar latexize-latex-command-char ?_)
;; (defvar latexize-latex-start-group-char ?$)
;; (defvar latexize-latex-end-group-char ?#)


(defun latexize-region (beg end)
  "Convert the region to LaTeX, preserving the font-lock colorization.
Returns a list with the LaTeX code and a list of faces used."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((faces (htmlize-faces-in-buffer)))
      (list (with-output-to-string
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (let* ((face (get-char-property (point) 'face))
                         (next-change (htmlize-next-change (point) 'face (line-end-position 1))))
                    (when (consp face)
                      ;; Choose the first face.  Here we might want to merge
                      ;; the faces.  In XEmacs, we might also want to takes
                      ;; into account all the `face' properties of all these
                      ;; extents overlapping next-change.  *sigh*
                      (setq face (car face)))
                    ;; FSF Emacs allows `face' property to contain arbitrary
                    ;; stuff.
                    (unless (symbolp face)
                      (setq face nil))
                    (when face
                      (princ (format "%cface%s%c" 
                                     latexize-latex-command-char
                                     (latexize-make-latex-name face)
                                     latexize-latex-start-group-char)))
                    (princ (buffer-substring-no-properties (point) next-change))
                    (when face
                      (princ (format "%c" latexize-latex-end-group-char)))
                    (goto-char next-change)
                    (when (eolp)
                      (terpri)
                      (forward-char 1))))))
            faces))))

(defun latexize-collect-slide-overlays (beg end)
  (interactive "r")
  (let ((slide-overlays nil)
	(line-num 0))
    (save-restriction
      (narrow-to-region beg end)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (looking-at "[0-9]+ ")
	    (let ((slide-num (read (current-buffer))))
	      (push (cons line-num slide-num) slide-overlays)
	      (delete-region (line-beginning-position) (+ (point) 1))))
	  (forward-line 1)
	  (incf line-num))))
    (reverse slide-overlays)))


(defun latexize-add-slide-overlays (beg end slide-overlays)
  (let ((prev 0))
    (save-restriction
      (narrow-to-region beg end)
      (save-excursion
	(goto-char (point-min))
	(while (not (null slide-overlays))
	  (let ((spec (pop slide-overlays)))
	    (forward-line (- (car spec) prev))
	    (insert (format "%cFromSlide%c%d%c" 
			    latexize-latex-command-char
			    latexize-latex-start-group-char
			    (cdr spec)
			    latexize-latex-end-group-char))
	    (setf prev (car spec))))))))

(defun latexize-collect-slide-highlights (beg end)
  (interactive "r")
  (let ((slide-highlights nil))
    (save-restriction
      (narrow-to-region beg end)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "__\\([0-9 ]+?\\)\\(..\\)\\(.+?\\)\\2" (point-max) t)
	  (let ((start-point (match-beginning 0))
		(slide-nums (match-string 1)))
	    (replace-match "\\3" t)
	    (let ((end-point (point)))
	      (push (cons (cons start-point end-point) (mapcar #'string-to-number (split-string slide-nums))) slide-highlights))))))
    slide-highlights))
      
(defun latexize-add-slide-highlights (beg end slide-highlights)
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (while (not (null slide-highlights))
	(let ((spec (pop slide-highlights)))
	  (goto-char (cdr (car spec)))
	  (dotimes (i (length (cdr spec)))
	    (insert latexize-latex-end-group-char))
	  (goto-char (car (car spec)))
	  (dolist (slide-num (cdr spec))
	    (insert (format "%chighlight%c%s%c%c" 
			    latexize-latex-command-char
			    latexize-latex-start-group-char
			    slide-num
			    latexize-latex-end-group-char
			    latexize-latex-start-group-char))))))))

(defun latexize-code (code-mode code-text)
  (unless (framep latexize-frame)
    (error "No latexize frame: did you latexize-initialize?"))
  (let ((latexize-result (with-temp-buffer
			   (set-window-buffer (frame-selected-window latexize-frame) (current-buffer))
			   (insert code-text)
			   (let* ((slide-overlays (latexize-collect-slide-overlays (point-min) (point-max)))
				  (slide-highlights (latexize-collect-slide-highlights (point-min) (point-max))))
			     (funcall code-mode)
			     (let ((font-lock-verbose nil))
                               (font-lock-default-fontify-buffer));;(font-lock-fontify-buffer)
			     (latexize-add-slide-highlights (point-min) (point-max) slide-highlights)
			     (latexize-add-slide-overlays (point-min) (point-max) slide-overlays))
			   (latexize-region (point-min) (point-max)))))
    (setq latexize-code-faces (union latexize-code-faces (second latexize-result)))
    (latexize-format-code-environment (first latexize-result))))

(defun latexize-format-code-environment (env-body)
  (concat "\\begin{SaveVerbatim}{\\CodeVerb}\n"
	  env-body
	  "\\end{SaveVerbatim}\n"))

(defun latexize-define-emacs-color-in-latex (latex-color-name emacs-color-name)
  (let ((rgb (color-values (or emacs-color-name "black"))))
    (format "\\xdefinecolor{%s}{rgb}{%f,%f,%f}"
	    latex-color-name
	    (/ (float (first rgb)) 65536)
	    (/ (float (second rgb)) 65536)
	    (/ (float (third rgb)) 65536))))

(defun latexize-initialize (bg-color fg-color &optional omit)
  (setq latexize-code-faces nil)
  (setq latexize-code-background bg-color)
  (setq latexize-code-foreground fg-color)
  (when (framep latexize-frame)
    (delete-frame latexize-frame))
  (setq latexize-frame
	(make-frame `((name . " *latexize frame*")
		      (visibility . nil)
		      (background-color . ,bg-color)
		      (foreground-color . ,fg-color))))
    (message "ooooooooooooo1")
  (unless omit
    (message "ooooooooooooo2")
    (concat (latexize-define-emacs-color-in-latex "codeBackground" bg-color) "\n"
            (latexize-define-emacs-color-in-latex "codeForeground" fg-color) "\n"
            "\\newcommand{\\CodeVerb}{tmpCode}\n"
            "\\newcommand{\\saveCodeIn}[1]{\\renewcommand{\\CodeVerb}{#1}}\n"
            "\\newcommand{\\boxCode}[1][\\CodeVerb]{\\begin{codeblock}\\BUseVerbatim{#1}\\end{codeblock}}\n"
            (format "\\fvset{commandchars=%c%c%c,fontsize=\\footnotesize,baselinestretch=.8,formatcom=\\color{codeForeground},baseline=t}\n"
                    latexize-latex-command-char
                    latexize-latex-start-group-char
                    latexize-latex-end-group-char))))

(defun latexize-define-code-faces ()
  (mapconcat (lambda (face)
               (let ((latex-name (latexize-make-latex-name face)))
                 (concat (latexize-define-emacs-color-in-latex latex-name (face-foreground face latexize-frame t))
                         "\n"
                         (format "\\newcommand{\\face%s}[1]{{\\color{%s}{}#1}}" latex-name latex-name))))
             latexize-code-faces
             "\n"))

(defun latexize-make-latex-name (emacs-name)
  ;; shorten face name
  (let ((face-name (replace-regexp-in-string (regexp-opt '("font-lock-" "-face")) "" (prin1-to-string emacs-name))))
    (mapconcat #'identity (split-string (capitalize face-name) "-") "")))


(defun latexize-linj-code (code-text)
  (latexize-code 'lisp-mode code-text))

(defun latexize-lisp-code (code-text)
  (latexize-code 'lisp-mode code-text))

(defun latexize-scheme-code (code-text)
  (latexize-code 'scheme-mode code-text))

(defun latexize-java-code (code-text)
  (latexize-code 'java-mode code-text))

(defun latexize-text (code-text)
  (latexize-code 'text-mode code-text))

;; (defun latexize-java-code (code-text)
;;   (latexize-code 'java-mode code-text))

(defun latexize-c-code (code-text)
  (latexize-code 'c-mode code-text))

(defun latexize-xml-code (code-text)
  (latexize-code 'sgml-mode code-text))

(defun latexize-sql-code (code-text)
  (latexize-code 'sql-mode code-text))


(provide 'latexize)
