;; -*- Mode: Emacs-Lisp -*-

;; stuff for (La)TeX editing
;; by Filipe Cabecinhas


;;(require 'auctex)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;;(setq TeX-engine 'xetex)
(setq-default TeX-engine 'xetex)

(setq-default TeX-PDF-mode t)
;;(TeX-global-PDF-mode t)
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)


(setq reftex-plug-into-AUCTeX t)
(setq TeX-debug-bad-boxes t
      TeX-toggle-debug-warnings t)

(setq reftex-label-alist
      '((nil ?f nil "\\figref{%s}" nil nil)))

;; Completion
(setq TeX-electric-escape nil)
;;(setq TeX-insert-braces t)              ; Chateia com comandos como o \ldots
(setq TeX-insert-braces nil)

;; Inverse search:
(setq TeX-source-specials-mode t)
(setq TeX-source-specials-view-start-server t)

;; (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))

(defun filcab-latex-mode-hook ()
  (turn-on-auto-fill)
;;  (highlight-changes-mode 1)
  (reftex-mode t)
  (flyspell-mode 1)
  (define-key LaTeX-mode-map "\C-c\C-c" 'maybe-elder-and-TeX-command-master)
  (setq-default reftex-cite-format "~\\cite{%l}")
;;  (flyspell-buffer)) ;; If it's too slow, just remove this
  (add-to-list 'TeX-output-view-style
	       `("^pdf$" "."
		 ,(concat "%(o?)" open-program " %o")))
  )


(in-platform windows-nt
  (eval-after-load "tex"
    '(add-to-list 'TeX-output-view-style
		  '("^pdf$" "."
		    "%(o?)start \"\" %o"))))

;; For auto-insert
(add-hook 'LaTeX-mode-hook 'auto-insert)
