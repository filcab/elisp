;;; c-stuff.el --- Functions and vars for editing C/C++/Objective-C/whatever

;; Copyright (C) 2010  Filipe Cabecinhas


;; TO SEE:

;; Different completion Interfaces

;; Clang completion:
(load-library "clang-completion-mode")

;; clang-format
(load (home-dir/ "clang-format.el"))
(global-set-key [C-M-tab] 'clang-format-region)
(global-set-key [C-M-S-tab] 'clang-format-buffer)

;; Here are a few tools available in CEDET/Semantic for performing completion.

;; Start inline completion

;;    M-x semantic-complete-analyze-inline
;; This is a command that does completion inline (underlining the target symbol) and allows TAB to be used for completion purposes.
;; Automatically starting inline completion in idle time

;;    M-x global-semantic-idle-completions-mode
;; This is a minor mode which runs semantic-complete-analyze-inline-idle during idle time. Instead of trying to complete the symbol immediately, it will just display the possible completions, and underline the current symbol the cursor is on.

;; Starting for inline completion when "." is pressed

;;   (define-key your-mode-map-here "." 'semantic-complete-self-insert)
;; Binding semantic-complete-self-insert to a key will insert that key's text, as per self-insert-command, and then run the inline completion engine if there is appropriate context nearby.

;; Speedbar completion mode

;;    M-x semantic-speedbar-analysis
;; This will start Speedbar in a special mode. In this mode it will analyze the cursor location, and provide intelligent references. Unlike inline completion, a raw list of options is provided and you just need to click on the one you want. Sometimes you need to press g to force an update.

;; Command based completion

;; The commands available (for binding to a key) are:

;; semantic-ia-complete-symbol
;; semantic-ia-complete-symbol-menu
;; semantic-ia-complete-tip
;; These commands are all very simple, and are designed as examples that a coder could examine to learn how to write their own completion interface.





;;;; C-mode hooks:
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (c-set-style "gnu")
            (setq c-basic-offset 2)
            (setq c-indent-level 2)
            (setq c-tab-always-indent 'complete)
            (setq tab-width 4)
            ;; indent com tabs
            (setq indent-tabs-mode nil)

            ;; Enable the Project management system
            ;; (global-ede-mode 1)

            ;; Enable prototype help and smart completion
;            (semantic-load-enable-code-helpers) ;; FILCAB: commented

            ;; Set up some keybindings for semantic
            ;; (local-set-key [(control return)] 'semantic-ia-complete-symbol)
            ;; (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
            ;; (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
            ;; (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)

            ;; Set up some self-insert for semantic completion when using C/C++
            ;; (local-set-key "." 'semantic-complete-self-insert)
            ;; (local-set-key ">" 'semantic-complete-self-insert)

            (show-prog-keywords)))


;;            (eldoc-mode))) ;; ElDoc


;; LLVM code conventions
(load (home-dir/ "llvm/emacs"))


(provide 'c-stuff)
;;; c-stuff.el ends here
