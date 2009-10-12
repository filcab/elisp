(require 'inf-lisp)
(require 'easymenu)


(defvar executable-binary-suffixes
  (if (memq system-type '(ms-dos windows-nt))
      '(".exe" ".com" ".bat" ".cmd" ".btm" "")
    '("")))         

(defun executable-find (command)
  "Search for COMMAND in exec-path and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  (let ((list exec-path)
        file)
    (while list
      (setq list
            (if (and (setq file (expand-file-name command (car list)))
                     (let ((suffixes executable-binary-suffixes)
                           candidate)
                       (while suffixes
                         (setq candidate (concat file (car suffixes)))
                         (if (and (file-executable-p candidate)
                                  (not (file-directory-p candidate)))
                             (setq suffixes nil)
                           (setq suffixes (cdr suffixes))
                           (setq candidate nil)))
                       (setq file candidate)))
                nil
              (setq file nil)
              (cdr list))))
    file))       
                      
;(setq inferior-lisp-program 
;      (if (not (eq system-type 'windows-nt))
;          (format "%s/xemacs/plasm_scheme" (getenv "PLASMHOME"))
;	"plasm_scheme"))
(setq inferior-lisp-program
	(executable-find "MzPlasm"))

(defvar internal-list nil)
(defvar is-active t)

(defun symbol-parser (str)
  (let ((lista (split-string str)))
    (setq internal-list
	  (if (string= (nth (1- (length lista)) lista) "#<PACKAGE USER>")
	      lista
;	      (reverse (cdr (reverse lista)))
	    lista))))

;(set-process-filter (get-buffer-process "*plasm*") nil)

;(setq comint-output-filter-functions (append comint-output-filter-functions '(symbol-parser)))

(defun create-popup-menu-vrml (symbol-list)
  (let ((i '0)
	(menu '("Scegli l'oggetto da esportare"))
	(len (length symbol-list)))
     (while (< i len)
;       (setq menu (append menu (list (make-vector 2 (nth i symbol-list)))))
       (setq menu (append menu (list (vector (nth i symbol-list) `(export-vrml-to-plasm ,(nth i symbol-list)) t ))))
       (setq i (1+ i)))
     menu))

(defun send-tab-to-inferior-plasm ()
  (interactive)
  (comint-dynamic-complete (inferior-lisp-proc)))
  

(defvar inferior-plasm-menubar '( "PLaSM" 
				  ["Export VRML        " export-vrml-popup is-active]
				  ["Set VRML version   " set-vrml-ver-emacs is-active]
		      ))

(define-derived-mode inferior-plasm-mode inferior-lisp-mode
  "PLaSM Interpreter"
  "This mode is for running a PLaSM interpreter as an inferior lisp process.
It is derived from the inferior-lisp-mode.
\\{inferior-lisp-mode-map}"
  (easy-menu-add inferior-plasm-menubar))

(define-key inferior-plasm-mode-map "\C-c\C-w" 'export-vrml-popup)
(define-key inferior-plasm-mode-map "\t" 'send-tab-to-inferior-plasm)

(defun inferior-plasm (cmd)
  "Run an inferior Lisp process, input and output via buffer `*plasm*'.
If there is a process already running in `*plasm*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program)
		       inferior-lisp-program)))
  (if (not (comint-check-proc "*plasm*"))
      (let ((cmdlist (inferior-lisp-args-to-list cmd)))
	(set-buffer (apply (function make-comint)
			   "plasm" (car cmdlist) nil (cdr cmdlist)))
	(inferior-plasm-mode)))
  (setq inferior-lisp-buffer "*plasm*")
  (delete-other-windows)
  (split-window-vertically)
  (pop-to-buffer "*plasm*"))

;;(define-function 'run-plasm 'inferior-plasm)

(defun plasm-eval-region (start end &optional and-go)
  "Send the current region to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-string (inferior-lisp-proc) "\(plasm \"")
  (comint-send-region (inferior-lisp-proc) start end)
  (comint-send-string (inferior-lisp-proc) "\"\)")
  (comint-send-string (inferior-lisp-proc) "\n")
  (if and-go (switch-to-lisp t)))

(defun plasm-eval-region-and-go (start end)
  "Send the current region to the inferior Lisp, and switch to its buffer."
  (interactive "r")
  (plasm-eval-region start end t))

(defun plasm-eval-buffer (&optional buffer)
  "Send all the current buffer to the inferior Lisp/PLaSM process for
evaling it."
  (let* ((plasm-buf (if (not (null buffer)) buffer (current-buffer))))
    (interactive "r")
    (plasm-eval-region (point-min plasm-buf) (point-max plasm-buf) t)))

(defun plasm-export-filter-vrml (process output)
  (let ((defdir (file-name-directory "."))
	(deffile (file-name-nondirectory "./pol.wrl"))
	fname
	)
    (interactive)
    (set-process-filter (get-buffer-process "*plasm*") nil)
    (setq fname (read-file-name "Inserire il nome del file : " defdir deffile))
    (comint-send-string (inferior-lisp-proc) fname)
    (comint-send-string (inferior-lisp-proc) "\n")))

(defun export-vrml-popup ()
"It exports the polyhedral complex value
of a plasm expression (default stored in last_value plasm name."
  (interactive)
;  (set-process-filter (get-buffer-process "*plasm*") 'symbol-parser)
;  (comint-send-string
;   (inferior-lisp-proc)
;   "(plasm::lista-simboli)")
;  (set-process-filter (get-buffer-process "*plasm*") nil)
  (popup-menu (create-popup-menu-vrml '("last_value"))))

(defun export-vrml-to-plasm (ogg)
"It exports the polyhedral complex value
of a plasm expression (default stored in last_value plasm name."
  (interactive)
  (set-process-filter (get-buffer-process "*plasm*") 'plasm-export-filter-vrml)
  (comint-send-string
   (inferior-lisp-proc)
   (format "(eval-plasm  \" export_vrml:(%s)\")\n" ogg))
  (sit-for '1)
  (switch-to-lisp t))

(defun set-vrml-ver-emacs ()
"It sets vrml exporting version"
    (setq ver
          (let ((vers (read-minibuffer "Type VRML version number (default 2):" "2")))
            (if (and (numberp vers)
                     (or (= vers 1)
                         (= vers 2)))
              vers
              2)))
      (interactive)
      (comint-send-string
       (inferior-lisp-proc) 
       (format "(io::set-vrml-ver %s )\n" ver)
       ))


(defvar plasm-mode-map (make-sparse-keymap)
  "Keymap used in PLaSM mode.")       

(define-key plasm-mode-map "\C-c\C-w" 'export-vrml-popup)
(define-key plasm-mode-map "\C-c\C-q" 'set-vrml-ver-emacs)
(define-key plasm-mode-map "\C-c\C-d" 'plasm-eval-buffer)
(define-key plasm-mode-map "\C-c\C-e" 'eval-buffer)
(define-key plasm-mode-map "\C-c\C-r" 'plasm-eval-region-and-go)
(define-key plasm-mode-map "\C-c\C-t" 'lisp-eval-region-and-go)
(define-key plasm-mode-map "\C-c\C-l" 'switch-to-lisp)
;;(define-key plasm-mode-map "\C-c\C-p" 'run-plasm)
(define-key plasm-mode-map [menu-bar] (make-sparse-keymap))  
(define-key plasm-mode-map [menu-bar plasm]
  (cons "PLaSM" (make-sparse-keymap "PLaSM")))
; (define-key plasm-mode-map [menu-bar plasm run-plasm]
;  '("Start Interpreter" . run-plasm))
 (define-key plasm-mode-map [menu-bar plasm plasm-eval-region-and-go]
  '("Eval PLaSM expression" . plasm-eval-region-and-go))
 (define-key plasm-mode-map [menu-bar plasm lisp-eval-region-and-go]
  '("Eval Scheme expression" . lisp-eval-region-and-go))  

;(put 'run-plasm 'menu-enable 'mark-active)
;(put 'plasm-eval-region-and-go 'menu-enable 'mark-active)
;(put 'lisp-eval-region-and-go 'menu-enable 'mark-active)  


(defun plasm-mode ()
  "PLaSM" 
  "Major mode for PLaSM language derived from lisp-mode one.
  \\{plasm-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map plasm-mode-map)
  (setq major-mode 'plasm-mode)
  (setq mode-name "PLaSM"))        

(setq auto-mode-alist
      (append '(("\\.psm$"  . plasm-mode))
	      auto-mode-alist))


(provide 'plasm-mode) 
