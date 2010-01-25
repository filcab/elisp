;; -*- Mode: Emacs-Lisp -*-

;; Stuff for editing scheme files.

(require 'quack)
(setq scheme-program-name "mzscheme")

(defvar plt-dir "~/dev/stuff/plt/")
(defvar quack-pltcollects-dir "~/dev/stuff/plt/collects/")



;(define-key scheme-mode-map [f1]
;  '(lambda ()
;     (interactive)
;     (ignore-errors
;       (let ((symbol (thing-at-point 'symbol)))
;	 (info "(r5rs)")
;	 (Info-index symbol)))))

