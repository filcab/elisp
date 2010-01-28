;; -*- Mode: Emacs-Lisp -*-

;; Stuff for editing scheme files.

(require 'quack)
(setq scheme-program-name "mzscheme")

(defvar plt-dir "~/dev/stuff/plt/")
(defvar quack-pltcollects-dir "~/dev/stuff/plt/collects/")

;; Fix scheme-compile-file for a recent PLT-Scheme (change the sent string)
(defun scheme-compile-file (file-name)
  "Compile a Scheme file FILE-NAME in the inferior Scheme process."
  (interactive (comint-get-source "Compile Scheme file: "
                                  scheme-prev-l/c-dir/file
                                  scheme-source-modes
                                  nil)) ; nil because COMPILE doesn't
                                        ; need an exact name.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc) (concat "(require (file \""
                                            file-name
                                            "\"\))\n")))

;(define-key scheme-mode-map [f1]
;  '(lambda ()
;     (interactive)
;     (ignore-errors
;       (let ((symbol (thing-at-point 'symbol)))
;	 (info "(r5rs)")
;	 (Info-index symbol)))))

