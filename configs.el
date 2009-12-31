;; -*- Mode: Emacs-Lisp -*-
;; Emacs configuration options
;; by Filipe Cabecinhas

;;;; Testing stuff
;;(setq-default command-frequency-table-file "~/.emacs.d/frequencies")
;;(require 'command-frequency)
;;(command-frequency-table-load)
;;(command-frequency-mode 1)
;;(command-frequency-autosave-mode 1)

;; When debugging
(setq debug-on-error t)

;; Utility functions/macros
(load "utils")

;; Setup load (and exec) paths stuff
(load "load-paths")

;; Private data (IRC, mail, etc.)
(defun load-private-data ()
  (load-private "private.el.gpg"))
;; We have private data for ERC, GNUS
(add-hook 'erc-mode-hook #'load-private-data)


;;;; Main configs
;; DON'T use iso-8859-* nor mac-roman nor cp1215 nor whatever
(prefer-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)
;; Terminal configurations
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

;; Use TeX style stuff for funny chars
(setq default-input-method "TeX")

;; Show stuff
(iswitchb-mode t)

;; ;; ido mode                          ;; NEW!
;; (ido-mode t)
;; (setq ;; Use it for many file dialogs
;;       ido-everywhere t
;;       ;; Don’t be case sensitive
;;       ido-case-fold t
;;       ;; If the file at point exists, use that
;;       ido-use-filename-at-point t
;;       ;; Or if it is an URL…
;;       ido-use-url-at-point t
;;       ;; Try dotfiles too
;;       ido-enable-dot-prefix t
;;       ;; Only do it if the file begins
;;       ;; with the typed characters
;;       ido-enable-prefix t
;;       ;; Even if TAB completes uniquely,
;;       ;; still wait for RET
;;       ido-confirm-unique-completion t
;;       ;; If the input does not exist,
;;       ;; don’t look in unexpected places.
;;       ;; I probably want a new file.
;;       ido-auto-merge-work-directories-length -1)

;; (setq ido-enable-flex-matching nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Major-modes configurations
;;(load "gnus-config")
(setq gnus-init-file "gnus-config")
(defun gnus-then-kill-emacs ()
  (interactive)
  (call-interactively 'gnus)
  (add-hook 'gnus-exit-gnus-hook 'save-buffers-kill-emacs))

;; find-file sudo'ed
(load "find-file-sudo")

;; Google and wikipedia searches
(load "google-wikipedia")

;; IRC stuff
(load "erc-config")

;; (La)TeX editing stuff
(load "latex-stuff")

;; Proced stuff (Emacs 23)
(defconst fc-proced-format
  '((user euid) pid tree pcpu pmem rss start state time pri nice
    (args comm))
  "My Proced format...")

;;(add-to-list 'proced-format-alist
;;             (cons 'fc-proced-format fc-proced-format))
;;
;; (setq-default proced-format fc-proced-format)

;; Misc configurations
(setq inhibit-startup-message t)
(setq backup-directory-alist
      `((".*" . ,(home-dir/ "backups/"))))
(setq-default indent-tabs-mode nil)

;; Lose the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Para ter as possiveis opcoes ao fazer find-file ou switch-buffer
;; Se calhar vejo o (require 'ido) sem o ido-mode, para usar só quando quero
;;(unless (< emacs-major-version 22)
;;  (require 'ido)
;;  (ido-mode t))

;; Syntax highlight e tal:
(require 'font-lock)
(setq font-lock-mode-maximum-decoration t)
(setq global-font-lock-mode t)
(if (fboundp 'transient-mark-mode)
    (transient-mark-mode t))
(show-paren-mode t)

;; Show line/column number in modeline
(line-number-mode      t)
(column-number-mode    t)

;; Show line numbers to the left of the buffer
(require 'linum)

;; Color theme
(require 'color-theme-autoloads "color-theme-autoloads")
(autoload 'color-theme-hober2 "color-theme-hober2" "Hober's 2nd color-theme")
(defun filcab-color-theme ()
  (interactive)
  (color-theme-initialize)
  (color-theme-dark-blue2)
  (color-theme-dark-green)) ;; To change some small stuff for the better

;; Display images
(auto-image-file-mode t)

;; Transparently edit compressed files
(auto-compression-mode t)

;; Scroll slowly
(setq scroll-step 1)

(setq-default show-trailing-whitespace t)

;; Emacs Lisp auto compile:
(setq-default auto-compile-when nil)
(setq-default auto-compile-flag 'compiledp-or-ask)
(setq-default auto-compile-remember nil)
(setq auto-compile-exclude '("private.el.gpg$")) ; Don't compile private files
(autoload 'auto-compile-mode "auto-compile" "Auto compile files" t)


;; EShell configs
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))


;; Doc-view for viewing PDFs
(require 'doc-view)

;; Small major mode configurations
(setq haskell-program-name "ghci")

;; SVN
(setq svn-status-svn-executable "/opt/local/bin/svn")

;; Load DVC (Distributed Version Control)
(load "dvc-load.el")


;; Key configurations
;; Give M-x compile a better shortcut
(global-set-key "\C-x\c" #'compile)

(global-set-key "\M-s" #'save-buffer)
(global-set-key "\M-\r" #'toggle-fullscreen)

(global-set-key "\C-z" 'undo)
(global-set-key "\M-g" 'goto-line)
;;(define-key global-map [(meta g)] 'goto-line)

;; Change default buffer list
(global-set-key  (kbd "C-x C-b") 'ibuffer-other-window)

;; Useful for LaTeX \input{} and stuff
(global-set-key "\C-xp" 'find-file-at-point)

(global-set-key [(control tab)] 'other-window) ;ctrl+(shift+)tab para os buffers
;;(global-set-key [(control shift tab)] 'other-buffer)

(global-set-key [(control x) (control r)] 'find-file-root)

;; Make dired sizes human-readable
(setq dired-listing-switches
      (concat dired-listing-switches "h"))

;; Setup dired to open or quicklook files
(defun dired-custom-dired-mode-hook ()
  (let ((open-dired (lambda (&optional arg)
                      (interactive "P")
                      (mapcar #'open-file-externally
                              (dired-get-marked-files nil nil))))
        (ql-dired (lambda (&optional arg)
                    (interactive "P")
                    (mapcar #'quicklook-file
                            (dired-get-marked-files nil nil)))))
    (define-key dired-mode-map "\C-cf" open-dired)
    (define-key dired-mode-map "\C-c\C-f" open-dired)
    (in-platform darwin
        (define-key dired-mode-map "\C-cq" ql-dired)
        (define-key dired-mode-map "\C-c\C-q" ql-dired)
        (define-key dired-mode-map "\C-cw" 'qlkill)
        (define-key dired-mode-map "\C-c\C-w" 'qlkill))))
(add-hook 'dired-mode-hook 'dired-custom-dired-mode-hook)


(setq require-final-newline t)

(in-platform darwin
  (require 'growl)
  ;; Mac OS X configurations
  (setq mac-pass-command-to-system nil)   ; avoid hiding with M-h
  (setq mac-command-modifier 'meta)       ; muscular memory, already in
                                          ; Carbon Emacs
  (setq mac-option-modifier nil))         ; make sure it's nil

;;; File templates:
(require 'autoinsert)
;;(auto-insert-mode)                        ; Adds hook to find-files-hook
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (home-dir/ "templates/"))
(setq auto-insert-query t)
(define-auto-insert "\.tex" "xelatex-template.tex")

;;elderdown'scroll
(require 'aml-elder)
(require 'latexize)


;; Archive-mode for RAR files
(add-to-list 'auto-coding-alist
             '("\\.\\(rar\\|RAR\\)\\'" . no-conversion))
(add-to-list 'auto-mode-alist
             '("\\.\\(rar\\|RAR\\)\\'" . archive-mode))

;; Finally, set up the hooks
(load "hooks")
