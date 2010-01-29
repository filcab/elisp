;; -*- Mode: Emacs-Lisp -*-

(when (null argv)
  (setq argv (directory-files "." nil "^[a-zA-Z0-9].*\.el$")))

(dolist (f argv)
  (when (string-equal (file-name-extension f) "el")
    (message (concat "Byte-compile file "
                     f ": "
                     (if (byte-compile-file f nil)
                         "OK!"
                       "Error!")))))

