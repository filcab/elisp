;;;; -*- Mode: Emacs-Lisp -*-


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (auto-compile-mode t)))


;;;; Lisp hooks:
(add-hook 'lisp-mode-hook 'filcab-lisp-mode-hook)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (lisp-mode-more-brackets)))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (inferior-slime-mode t)))


;;;; C-mode hooks:
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (c-set-style "gnu")
            (setq c-basic-offset 2)
            (setq c-indent-level 2)
            (setq c-tab-always-indent t)
            (setq tab-width 4)
            ;; indent com tabs
            (setq indent-tabs-mode nil)
            (eldoc-mode))) ;; ElDoc

;;;; Java hooks:
(add-hook 'java-mode-common-hook
          (lambda ()
            (c-set-style "k&r")
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (setq c-basic-offset 4)
            (setq c-indent-level 4)
            (setq c-tab-always-indent nil)
            (setq tab-width 4)
            ;; indent com tabs
            (setq indent-tabs-mode nil)))

;;;; Haskell hooks
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;;; (La)TeX hooks
(add-hook 'LaTeX-mode-hook 'filcab-latex-mode-hook)
