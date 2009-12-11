;; -*- Mode: Emacs-Lisp -*-

;(require 'erc-auto)

(setq erc-modules
  '(autojoin button completion fill irccontrols
    match menu netsplit noncommands readonly ring
    scrolltobottom services smiley stamp track))

(erc-spelling-mode 1)

(setq erc-track-use-faces t)
(setq erc-echo-notices-in-minibuffer-flag t)
;(setq erc-auto-query 'bury)  ; private messages displayed in another buffer

;; Notify when my nick is mentioned
(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (growl (format "%s in %s"
                 ;; Username of sender
                 (car (split-string nickuserhost "!"))
                 ;; Channel
                 (or (erc-default-target) "#unknown"))
         ;; Remove duplicate spaces
         (replace-regexp-in-string " +" " " message)))

(add-hook 'erc-text-matched-hook 'my-notify-erc)


;; Log stuff
(setq erc-log-channels-directory "~/Documents/logs/erc/")
(setq erc-save-buffer-on-part t)
(setq erc-log-insert-log-on-open nil)

(defun erc-save-buffers-in-logs ()
  (interactive)
  (mapc (lambda(buf)
	  (save-excursion
	    (set-buffer buf)
	    (erc-save-buffer-in-logs)))
	(erc-buffer-filter (lambda() t))))

;; Save buffers when exiting emacs
(defadvice save-buffers-kill-emacs
   (before save-logs-before-save-buffers-kill-emacs (&rest args) activate)
   'erc-save-buffers-in-logs)

(defadvice save-some-buffers
  (before save-logs-before-save-some-buffers (&rest args) activate)
  'erc-save-buffers-in-logs)

;; auto-save logs on every line
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-send-post-hook   'erc-save-buffer-in-logs)

(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))

(setq erc-input-line-position -1)

(setq erc-timestamp-format "[%H:%M] ")

;; Doesn't work, yet.
;(setq erc-fill-function erc-fill-static)

;; Don't track stuff from the server
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; Set a per-channel prompt
(setq erc-prompt
      (lambda ()
	(if (and (boundp 'erc-default-recipients) (erc-default-target))
	    (erc-propertize (concat (erc-default-target) ">")
			    'read-only t
			    'rear-nonsticky t
			    'front-nonsticky t)
	  (erc-propertize (concat "ERC>")
			  'read-only t
			  'rear-nonsticky t
			  'front-nonsticky t))))

;; Macro to create ERC connection-functions
(defmacro de-erc-connect (command server port nick)
  "Create interactive command `command', for connecting to an IRC server. The
      command uses interactive mode if passed an argument."
  (fset command
	`(lambda (arg)
	   (interactive "p")
	   (if (not (= 1 arg))
	       (call-interactively 'erc)
	     (erc :server ,server :port ,port :nick ,nick)))))

(de-erc-connect erc-freenode "irc.freenode.net" 6667 "filcab42")
(de-erc-connect erc-llvm "irc.oftc.net" 6667 "filcab")
(de-erc-connect erc-quakenet "irc.quakenet.org" 6667 "filcab")


;(setq erc-default-connections '(erc-freenode erc-llvm))
;(setq erc-default-connections '(erc-freenode))
(setq erc-default-connections '())
(defun irc ()
  "Connect to IRC."
  (interactive)
  (let ((connections erc-default-connections))
    (when connections
      (call-interactively (car connections))
      (mapcar (lambda (connection)
                (sit-for 1)
                (call-interactively connection))
              (cdr connections)))))


;; Passwords
(require 'erc-services)
(erc-services-mode 1)

;; ;; Freenode changed the line to warn about registered nicks
;; ;; Change it back. I want to be warned when this hack becomes useless
;; ;; Version 5.3 no longer needs it
;; (when (string= (substring (erc-version) 12 15) "5.2")
;;   (let ((nickserv (assoc 'freenode erc-nickserv-alist)))
;;     (if (string= "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>"
;;                  (nth 2 nickserv))
;;         (setcar (cddr nickserv)
;;                 "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>")
;;       (message "Freenode NickServ IDENTIFY hack is now useless"))))
;;
;; ;; And now, the OFTC hack
;; (let ((nickserv (assoc 'OFTC erc-nickserv-alist)))
;;   (if (string= "type\\s-/msg\\s-NickServ\\s-IDENTIFY\\s-password."
;;                (nth 2 nickserv))
;;       (setcar (cddr nickserv)
;;               "services\\s-with\\s-the\\s-IDENTIFY\\s-command")
;;     (message "OFTC NickServ IDENTIFY hack is now useless")))


(defun erc-cmd-PLAYING ()
  "Tells the people what I'm listening to"
  (erc-cmd-ME (concat " " "is listening to "
                      (current-itunes-song))))



;;(load-private "~/.ercpass")
(setq erc-prompt-for-nickserv-password nil)

;;(load-private "erc-private.el")

