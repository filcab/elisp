;;; aml-elder.el --- 
;;;----------------------------------------------------------------------
;; Author: Antonio Menezes Leitao <aml@gia.ist.utl.pt>
;; Created on: Sat Jul  8 10:03:31 2006
;; Keywords: 
;;
;; Copyright (C) 2006 Antonio Menezes Leitao

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

(require 'elder)

;;This will use the base mode for any <file>.<base-mode>.e

(add-to-list 'auto-mode-alist '("\\.e\\'" ignore t))

;;Utility functions for elder

(defun e-replace-text (area)
  (if (string-match "\\(.+\\)$" area)
      (let ((name (substring area (match-beginning 1) (match-end 1)))
	    (replacement (substring area (1+ (match-end 0)))))
	(end-of-line)
	(while (re-search-forward name nil t)
	  (replace-match replacement t t)))
    (error "Couldn't identify the text to replace")))

(defun e-replace-rect (area)
  (let ((strings (list))
	(pos 0))
    (while (string-match "\\(.+\\)$" area pos)
      (setq pos (match-end 0))
      (push (substring area (match-beginning 1) (match-end 1)) strings))
    (setq strings (nreverse strings))
    (if (endp strings)
      (error "Couldn't identify the text to replace")
      (let ((name (first strings))
	    (replacements (rest strings)))
	(end-of-line)
	(while (re-search-forward name nil t)
	  (replace-match "" t t)
	  (let ((end (point))
		(start (save-excursion (beginning-of-line) (point))))
	    (let ((fill (buffer-substring start end)))
	      (delete-region start (1+ end))
	      (dolist (str replacements)
		(insert fill)
		(insert str)
		(insert "\n")))))
	""))))


;;And now, hook into opening and closing files.
(defvar *dont-check-elder-file* nil)

(defun find-file-for-elder ()
  "Function for `find-file-hooks' activating checking of elder files."
  (unless *dont-check-elder-file*
    (when buffer-file-name
      (let ((elder-file-name (concat buffer-file-name ".e")))
	(cond ((file-exists-p elder-file-name)
	       (when (y-or-n-p "This seems to be an elder processed file.  Shall I open the source instead? ")
		 ;;(setq buffer-read-only t) ;;protect current file
		 ;;even better, kill it
		 (kill-buffer (get-file-buffer buffer-file-name))
		 (find-file elder-file-name)))
	      ;; 	    ((string= (file-name-extension buffer-file-name) ".e") ;;this is the elder file
	      ;;  	     (set-auto-mode))
	      )))))

(defun save-file-for-elder ()
  "Function for `find-file-hooks' activating checking of elder files."
  (when buffer-file-name
    (when (string= (file-name-extension buffer-file-name) "e") ;;this is the elder file
      (when (y-or-n-p "Process this elder buffer after save?")
        (if (string= (file-name-extension (file-name-sans-extension buffer-file-name)) "tex")
            (elder-etex-this-file)
	  (let ((buff (get-file-buffer buffer-file-name)))
	    (elder buffer-file-name)
	    ;; (kill-buffer buff)
	    ))))))

(add-hook 'find-file-hooks 'find-file-for-elder)
(add-hook 'after-save-hook 'save-file-for-elder)


(defun maybe-elder-and-TeX-command-master ()
  (interactive)
  (when (string-match "\\.e\\'" buffer-file-name)
    (let ((tex-file (substring buffer-file-name 0 (match-beginning 0))))
      (elder-etex-this-file)
      (let ((*dont-check-elder-file* t))
	(let ((buffer (find-buffer-visiting tex-file)))
	  (when buffer
	    (kill-buffer buffer)))
	(find-file-read-only-other-window tex-file))))
  (TeX-command-master))

;;
(defun jump-over (text)
  "Remove the region."
  "")

;;Tables
;;We need a model:

(defstruct table-cell
  row
  column
  element
  bold)

(defstruct table-row
  cells)

(defstruct table
  cell-alignment
  cell-key
  rows)

(defmacro* table ((&key column-alignment (cell-key '#'table-cell-element)) &rest args)
  `(make-table 
    :rows (list ,@args)
    :cell-key ,cell-key))

(defun titles (&rest args)
  (make-table-row
   :cells (mapcar #'(lambda (arg)
		      (make-table-cell :element arg :bold t))
		  args)))

(defun line (&rest args)
  (make-table-row 
   :cells (mapcar #'(lambda (arg)
		      (make-table-cell :element arg))
		  args)))

(defun check-table-layout (table)
  (let ((columns-cols
	 (mapcar (lambda (row)
		   (length (table-row-cells row)))
		 (table-rows table))))
    (let ((first (first columns-cols)))
      (dolist (row (rest columns-cols))
	(assert (= first row) () "There are rows with a different number of collumns"))))
  ;;assign position
  (loop for i upfrom 0
	for row in (table-rows table)
	do (loop for j upfrom 0
		 for cell in (table-row-cells row)
		 do (setf (table-cell-row cell) i
			  (table-cell-column cell) j))))

(defun transpose-table (table)
  (make-table
   :cell-key (table-cell-key table)
   :cell-alignment (table-cell-alignment table)
   :rows (apply #'mapcar*
		#'(lambda (&rest args)
		    (make-table-row :cells args))
		(mapcar #'table-row-cells (table-rows table)))))

(defun* fmt (fmt-str &rest args)
  (princ (apply #'format fmt-str args)))

(defun print-table (table)
  (check-table-layout table)
  (with-output-to-string 
    (let ((columns (length (table-row-cells (first (table-rows table))))))
      (fmt "\\begin{tabular}{")
      (dotimes (i columns)
	(fmt "@{}c@{}"))
      (fmt "}\n"))
    (dolist (row (table-rows table))
      (print-table-row table row))
    (fmt "\\end{tabular}\n")))

(defun print-table-row (table row)
  (let ((first t))
    (dolist (cell (table-row-cells row))
      (if first
	(setf first nil)
	(fmt " & "))
      (print-table-cell table row cell))
    (fmt "\\\\\n")))

(defun print-table-cell (table row cell)
  (let ((contents (funcall (table-cell-key table) cell)))
    (cond (nil ;;check here multirows, multicolumns, different alignment, etc
	   (fmt "\\multicolumn{c}{")
	   (fmt "%s" contents)
	   (fmt "}"))
	  ((table-cell-bold cell)
	   (fmt "\\textbf{")
	   (fmt "%s" contents)
	   (fmt "}"))
	  (t
	   (fmt "%s" contents)))))

;;TO BE FINISHED

;; (defun compact-lines-columns (types-ranges)
;;   (mapcar #'cons
;; 	  (mapcar #'compact-multicolumns (compact-multi-lines (mapcar #'first types-ranges)))
;; 	  (mapcar #'rest types-ranges)))

;; (defun extend-right (item extension)
;;   (multiple-value-bind (lines cols)
;;       (if (consp extension)
;; 	(values-list (rest extension))
;; 	(values 1 1))
;;     (if (consp item)
;;       (list (first item) (second item) (+ (third item) cols))
;;       (list item lines (1+ cols)))))

;; (defun compact-multi-lines (types)
;;   (if (endp (first types))
;;     types
;;     (mapcar #'cons (compact-lines (mapcar #'first types)) (compact-multi-lines (mapcar #'rest types)))))

;; (defun compact-lines (types)
;;   (maplist #'(lambda (types)
;; 	       (cond ((eq (first types) '&)
;; 		      '_)
;; 		     ((eq (first (rest types)) '&)
;; 		      (list (first types)
;; 			    (loop :for i :from 1
;; 				  :for type :in (rest types)
;; 				  :while (eq type '&)
;; 				  :finally (return i))
;; 			    1))
;; 		     (t
;; 		      (first types))))
;; 	   types))

;; (defun compact-multicolumns (types)
;;   "Translates from ... T & & ... to ... (T 1 3) ..."
;;   (cond ((endp types) (list))
;; 	((extend-right-p (first types))
;; 	 (error "Unmatched right extension"))
;; 	((extend-right-p (second types))
;; 	 (compact-multicolumns
;; 	  (if (empty-cell-p (first types))
;; 	    (cons (first types)
;; 		  (compact-multicolumns (cons (first types) (rest (rest types)))))
;; 	    (cons (extend-right (first types) (second types))
;; 		  (rest (rest types))))))
;; 	((consp (first types))
;; 	 (cons (first types)
;; 	       (compact-multicolumns (rest types))))
;; 	(t
;; 	 (cons (list (first types) 1 1)
;; 	       (compact-multicolumns (rest types))))))

(provide 'aml-elder)



;; (print-table
;;  (let ((b "Basic") (p "Pascal") (f "Fortran") (c "C") (fl "FranzLisp") (s "Scheme") (ll "LeLisp") (z "ZetaLisp") (el "EmacsLisp") 
;;        (cl "CommonLisp") (e "Eiffel") (cc "C++") (sm "Smalltalk") (oc "Objective-C") (j "Java"))
;;    (table 
;;     (:cell-key (lambda (c) 
;; 		 (if (and (> (table-cell-row c) 0) (> (table-cell-column c) 0))
;; 		   (format "\\circle{%s}" (table-cell-element c))
;; 		   (table-cell-element c))))
;;     (titles    ""  b   p   f   c   fl  s   ll  z   el  cl  e   cc  sm  oc  j)
;;     (line    1981  5   0   0   0   0   0   0   0   0   0   0   0   0   0   0)
;;     (line    1982 10   0   0   0   0   0   0   0   0   0   0   0   0   0   0)
;;     (line    1983 10   0   0   0   0   0   0   0   0   0   0   0   0   0   0)
;;     (line    1984  0   7   3   0   0   0   0   0   0   0   0   0   0   0   0)
;;     (line    1985  0  10   5   0   0   0   0   0   0   0   0   0   0   0   0)
;;     (line    1986  0   0   3  10   5   5   5   0   3   0   0   0   0   0   0)
;;     (line    1987  0   0   0  10  10  10  10   0   5   0   0   0   0   0   0)
;;     (line    1988  0   0   0  10   5  10   5  10   8   0   0   0   0   0   0)
;;     (line    1989  0   0   0  10   0  10   0  10  10   0   0   0   0   0   0)
;;     (line    1990  0   0   0   0   0   5   0  10  10   0   3   4   8   5   0)
;;     (line    1991  0   0   0   0   0   0   0   0  10  10   0   6   0  10   0)
;;     (line    1992  0   0   0  10   0   0   0   0  10  10   0   0   0  10   0)
;;     (line    1993  0   0   0   0   0   0   0   0  10  10   0   0   0   5   0)
;;     (line    1994  0   0   0   0   0   0   0   0  10  10   0  10   0   0   0)
;;     (line    1995  0   0   0   0   0   0   0   0  10  10   0  10   0   0   0)
;;     (line    1996  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    1997  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    1998  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    1999  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2000  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2001  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2002  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2003  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2004  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2005  0   0   0   0   0   0   0   0  10  10   0   0   0   0  10)
;;     (line    2006  0   0   0   0   0   0   0   0  10  10   0   0  10   0  10))))

