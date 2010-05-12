;; -*- Mode: Emacs-Lisp -*-

;; stuff for (La)TeX editing
;; by Filipe Cabecinhas

(require 'utils)
(require 'load-paths)

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

;; Programs which open PDF files
(in-platform darwin
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline %n %o %b")
          ("open" "open %o")))

  (setq TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "Skim")
          (output-html "open"))))


;; FIXME: TeX-master-file doesn't work here?
(defadvice TeX-command-master
  (after TeX-command-master-refresh-doc-view
         preactivate compile)
  "Refresh doc-view buffers for this TeX file."
  (interactive "P")
  ;; (let ((master (TeX-master-file "pdf")))
  ;;   (message "Advice is running. master = %s" master)
  ;;   (dolist (buf (buffer-list 'files-only))
  ;;     (message "I have a buffer: %s... same? %S" (buffer-name buf)
  ;;              (string-equal master (buffer-name buf)))
  ;;     (when (string-equal master (buffer-name buf))
  ;;       (message "Advice is running... found a buffer!")
  ;;       (save-window-excursion
  ;;         (switch-to-buffer buf)
  ;;         (revert-buffer 'ignore-auto 'noconfirm))))))
  (dolist (f (frame-list))
    (dolist (win (window-list f))
      (let ((buf (window-buffer win)))
        (when (and (buffer-file-name buf)
                   (string-equal (file-name-extension (buffer-file-name buf))
                                 "pdf"))
          (save-window-excursion
            (switch-to-buffer buf)
            (revert-buffer 'ignore-auto 'noconfirm)))))))

;; FIXME: If the next TeX command is "View", run it!
;; (defadvice TeX-command-master
;;   (after TeX-command-master-refresh-external-app
;;          preactivate compile)
;;   "Refresh the pdf viewer reading the output file"
;;   (interactive "P")
;;   (if (string-equal TeX-command



(defun filcab-latex-mode-hook ()
  (turn-on-auto-fill)
  ;;(highlight-changes-mode 1)
  (reftex-mode t)
  (flyspell-mode 1)
  ;; Enables our maybe-elder-and-TeX-command-master, to pre-process files
  ;; Also activates our "refresh-doc-view" advice, to reload PDFs
  (ad-activate 'TeX-command-master t)

  (setq-default reftex-cite-format "~\\cite{%l}")
;;  (flyspell-buffer)) ;; If it's too slow, just remove this
;;  (add-to-list 'TeX-output-view-style
;;	       `("^pdf$" "."
;;		 ,(concat "%(o?)" open-program " %o")))
  )

(defun master-file-PDF-other-window ()
  (interactive)
  (let ((pdf (concat (TeX-master-directory) "/"
                     (TeX-master-file "pdf"))))
    (if (file-exists-p pdf)
        (save-excursion
          (other-window 1)
          (find-file pdf))
      (message "Couldn't find master PDF file: %s" pdf))))

(in-platform windows-nt
  (eval-after-load "tex"
    '(add-to-list 'TeX-output-view-style
		  '("^pdf$" "."
		    "%(o?)start \"\" %o"))))

;; For auto-insert
(add-hook 'LaTeX-mode-hook 'auto-insert)

;; Bind master-file-PDF-other-window in latex-mode
;;(local-set-key 'latex-mode-map

(provide 'latex-stuff)
