;; -*- Mode: Emacs-Lisp -*-

;; stuff for slime
;; by Filipe Cabecinhas

;; Slime stuff
;(add-to-load-path "~/dev/slime")
(add-to-load-path (expand-file-name "~/dev/slime/contrib"))
(load "slime")
(require 'slime)
(slime-setup '(slime-asdf slime-fancy slime-xref-browser))
;(slime-setup '(slime-fancy slime-asdf))




;(setq inferior-lisp-program "alisp")
;(setq inferior-lisp-program (home-dir/ "toolchains/bin/sbcl --no-linedit"))
;(setq inferior-lisp-program (home-dir/ "toolchains/bin/sbcl"))

(setq slime-lisp-implementations
      '((allegro ("/Applications/Dev/AllegroCL/alisp"))
        (cmucl ((home-dir/ "toolchains/bin/lisp")))
        (openmcl ((home-dir/ "toolchains/bin/openmcl64")))
        (sbcl ((home-dir/ "toolchains/bin/sbcl") "--no-linedit"))))

(setq slime-default-lisp 'allegro)

(defun invoke-cl (slime-default-lisp)
;  (hack-slime-locally)
  (slime))

(defun allegro ()
  (interactive)
  (invoke-cl 'allegro))

(defun cmucl ()
  (interactive)
  (invoke-cl 'cmucl))

(defun openmcl ()
  (interactive)
  (invoke-cl 'openmcl))

(defun sbcl ()
  (interactive)
  (invoke-cl 'sbcl))

(defun hack-slime-remotely (host)
  (interactive "sHost: ")
  (setq slime-translate-to-lisp-filename-function
        `(lambda (file-name)
           (if (string-match (concat "^\\(/\\(scp:\\)?" ,host ":\\)") file-name)
               (subseq file-name (length (match-string 1 file-name)))
               file-name))
        slime-translate-from-lisp-filename-function
        `(lambda (file-name)
           (concat "/" ,host ":" file-name))))

(defun hack-slime-locally ()
  (interactive)
  (setq slime-translate-to-lisp-filename-function 'identity
        slime-translate-from-lisp-filename-function 'identity))

;;; remote slime connections

(defmacro define-slime-connector (name port remote-host-name)
  `(defun ,(intern (format "slime-%s" (symbol-name name))) ()
     (interactive)
     (slime-connect "127.0.0.1" ,port)
     (hack-slime-remotely ,remote-host-name)))

(define-slime-connector plouffe 4005 "plouffe.local")


;; Misc configs
(setq slime-startup-animation nil)  ; startup animation is slow
(setq common-lisp-hyperspec-root
      (concat "file://" (home-dir/ "dev/lisp/docs/Hyperspec-7.0/HyperSpec")))

;; Teclas para o slime:
(define-key slime-mode-map (kbd "RET") 'newline-and-indent)
;(define-key slime-mode-map (kbd "TAB")
;  'slime-fuzzy-indent-and-complete-symbol)
;(define-key slime-scratch-mode-map (kbd "RET") 'newline-and-indent)
;(define-key slime-scratch-mode-map (kbd "TAB")
;  'slime-fuzzy-indent-and-complete-symbol)

;(setq lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(defun lisp-mode-more-brackets ()
  (modify-syntax-entry ?\[ "(]  ")
  (modify-syntax-entry ?\] ")[  ")
            
  (modify-syntax-entry ?\{ "(}  ")
  (modify-syntax-entry ?\} "){  "))


(defun filcab-lisp-mode-hook ()
  ;; obrigatorio slime-mode
  (slime-mode t)
  (require 'lisp-mode)
  ;; newline and indent quando se carrega em C
  ;;(local-set-key "\r" 'newline-and-indent)
  ;; indent decente para cl
  (setq lisp-indent-function
        'common-lisp-indent-function)
  (setq indent-tabs-mode nil)
  (lisp-mode-more-brackets))


(defun slime-send-dwim (arg)
  "Send the appropriate forms to CL to be evaluated."
  (interactive "P")
  (save-excursion
    (cond
      ;;Region selected - evaluate region
      ((not (equal mark-active nil))
       (copy-region-as-kill-nomark (mark) (point)))
      ;; At/before sexp - evaluate next sexp
      ((or (looking-at "\s(")
	   (save-excursion
	     (ignore-errors (forward-char 1))
	     (looking-at "\s(")))
       (forward-list 1)
       (let ((end (point))
	     (beg (save-excursion
		    (backward-list 1)
		    (point))))
	 (copy-region-as-kill-nomark beg end)))
      ;; At/after sexp - evaluate last sexp
      ((or (looking-at "\s)")
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\s)")))
       (if (looking-at "\s)")
	   (forward-char 1))
       (let ((end (point))
	     (beg (save-excursion
		    (backward-list 1)
		    (point))))
	 (copy-region-as-kill-nomark beg end)))
      ;; Default - evaluate enclosing top-level sexp
      (t (progn
	   (while (ignore-errors (progn
				   (backward-up-list)
				   t)))
	   (forward-list 1)
	   (let ((end (point))
		 (beg (save-excursion
			(backward-list 1)
			(point))))
	     (copy-region-as-kill-nomark beg end)))))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (yank)
    (if arg (progn
	      (slime-repl-return)
	      (other-window 1)))))

;(define-key lisp-mode-map [f7] 'slime-send-dwim)
;(define-key lisp-mode-map [f8] '(lambda ()
;				  (interactive)
;				  (slime-send-dwim 1)))

;; Lispdoc
(defun lispdoc ()
  "searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not (word-at-point))
             (not symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))


