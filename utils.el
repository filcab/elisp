;; -*- Mode: Emacs-Lisp -*-

(defun load-private (file &optional NOMESSAGE NOSUFFIX MUST-SUFFIX)
  "Just like #'load, when NOERROR=t, but warns about not loading."
  (if (load file t NOMESSAGE NOSUFFIX MUST-SUFFIX)
      t
    (message (concat "Couldn't load private file \""
                     file "\"."))
    nil))

;; Macros for platform independence
(defmacro setq-platform (symbol defs)
  `(setq ,symbol
         ,(or (cdr (assoc system-name defs))
              (cdr (assoc system-type defs))
              (error
               "System name (%s) and System type (%s) not found in:\n  %s."
               system-name system-type defs))))

(defmacro in-platform (platform &rest body)
  (if (or (string= system-name platform)
          (eq system-type platform))
      `(progn ,@body)))

(defmacro in-platforms ((platform &rest body) &rest rest)
  (cond ((or (string= system-name platform)
             (eq system-type platform))
         `(progn ,@body))
        ((null rest)) ;; Stop
        (t `(in-platforms ,@rest))))


(defun add-to-list* (list-var elements &optional append compare-fn)
  "Add several elements to list-var (using add-to-list)."
  (let ((compare-fn (or compare-fn 'equal)))
    (mapcar (lambda (element)
	      (add-to-list list-var element append compare-fn))
	    elements)
    (symbol-value list-var)))

(defun add-to-load-path (&rest paths)
  "Adds every path (after expansion) to the variable 'load-path"
  (add-to-list* 'load-path
                (mapcar (lambda (path)
                          (expand-file-name path))
			paths)))

(defun add-to-exec-path (&rest paths)
  "Adds every path (after expansion) to the variable 'exec-path"
  (add-to-list* 'exec-path
                (mapcar (lambda (path)
                          (expand-file-name path))
			paths)))

(defun toggle-fullscreen ()
  "Toggles fullscreen in emacs"
  (interactive)
  (set-frame-parameter nil
                       'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                           'fullboth)))

(defun copy-line ()
  "Copy line to kill ring."
  (interactive)
  (copy-region-as-kill (line-beginning-position)
		       (line-end-position)))

(defun current-itunes-song ()
  "Returns the music currently playing in iTunes"
  (do-applescript
   "tell application \"iTunes\"
         set currentTrack to the current track
         set artist_name to the artist of currentTrack
         set song_title to the name of currentTrack
         return artist_name & \" - \" & song_title
    end tell"))

(defun itunes-now-playing ()
  "Echoes the music currently playing in iTunes"
  (interactive)
  (message (current-itunes-song)))

(defun itunes-next-track (p)
  (interactive "p")
  (setq p (if (zerop p) 1 p))
  (dotimes (i p)
    (applescript
     "tell application \"iTunes\"
           next track
      end tell")))

(defvar macosx-open-program
  "open"
  "Default program to use in MacOS X to open files externally")

(defvar open-program
  macosx-open-program
  "Default program to use to open files externally")

(defun open-file-externally (file)
  (interactive "fOpen file externally: ")
  (process-file open-program nil t nil (expand-file-name file)))
;;  (process-file macosx-open-program))

(defun quicklook-file (file)
  (interactive "fQuicklook file: ")
  (start-process "ql"
                 nil
                 "qlmanage"
                 "-p" (expand-file-name file)))

(defalias 'ql 'quicklook-file)

(defun quicklook-kill ()
  (interactive)
  (start-process "qlkill"
                 nil
                 "killall"
                 "qlmanage"))

(defalias 'qlkill 'quicklook-kill)

(defun get-region-or-ask-string (prompt)
  "If the mark is active, gets the text inside the
region. Otherwise, ask for a string."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string prompt)))


(defun fill-buffer (&optional p)
  (interactive "p")
  (save-excursion
    (fill-region (point-min) (point-max) p)))

(defun hide-menus ()
  (interactive)
  (let ((menu-bar-old (lookup-key global-map [(menu-bar)])))
    (define-key global-map [(menu-bar)] nil)
    (sit-for 3) ;; That should be enough...
    (define-key global-map [(menu-bar)] menu-bar-old)))

;; Create the list in the background.
;; Subsequent executions should take around 0.2 secs
(in-platform macosx
	     (start-process "app-switcher-list"
			    nil
			    "mdfind"
			    "kMDItemKind == Application"))

(defun app-list ()
  (interactive)
;  (read-from-string
  (with-output-to-string
    (call-process "osascript"
                  nil
                  standard-output
                  nil
                  "-e"
                  "tell application \"System Events\" to \
                        set the_apps to name of application processes \
                        whose background only is false")))


(defun app-switch ()
  (interactive)
  (let ((app (get-region-or-ask-string
              "Which application should I switch to? ")))
    (applescript "tell application \""
                 app
                 "\" to activate")))

(defun applescript (&rest texts)
  (let ((text (apply #'concat
                     texts)))
    (start-process "applescript"
                   nil
                   "osascript" "-e"
                   text)))

