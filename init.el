;; -*- Mode: Emacs-Lisp -*-
;; .emacs in farnsworth's Mac OS X
;; by Filipe Cabecinhas

(setq debug-on-error t)

(defconst home-dir (expand-file-name "~/.emacs.d"))
(defun home-dir/ (&optional file)
  (setq file (or file ""))
  (concat home-dir "/" file))

(add-to-list 'load-path home-dir)

(load "configs")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "1c5d1c590603119feb2b954caf6d55294f97f4cc")
 '(quack-programs (quote ("~/dev/stuff/plt/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M
    errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-engine . xetex)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
