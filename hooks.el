;;;; -*- Mode: Emacs-Lisp -*-


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (auto-compile-mode t)

            (show-prog-keywords)))


;;;; Lisp hooks:
(add-hook 'lisp-mode-hook 'filcab-lisp-mode-hook)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (if (fboundp 'lisp-mode-more-brackets)
                (lisp-mode-more-brackets))
            (show-prog-keywords)))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (if (fboundp 'inferior-slime-mode)
                (inferior-slime-mode t))
            (show-prog-keywords)))



;;;; Java hooks:
(add-hook 'java-mode-common-hook
          (lambda ()
            (c-set-style "k&r")
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (setq c-basic-offset 4)
            (setq c-indent-level 4)
            (setq c-tab-always-indent 'complete)
            (setq tab-width 4)
            ;; indent com tabs
            (setq indent-tabs-mode nil)
            (show-prog-keywords)))

;;;; Haskell hooks
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'show-prog-keywords)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;;; (La)TeX hooks
(add-hook 'LaTeX-mode-hook 'filcab-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'show-prog-keywords)

;; Text mode hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(provide 'hooks)
