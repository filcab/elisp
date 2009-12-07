;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(when (null argv)
  (setq argv (directory-files "." nil "^[a-zA-Z0-0].*\.el$")))

(dolist (f argv)
  (when (string-equal (file-name-extension f) "el")
    (message (concat "Byte-compile file "
                     f ": "
                     (if (byte-compile-file f nil)
                         "OK!"
                       "Error!")))))

