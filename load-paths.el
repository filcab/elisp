;; -*- Mode: Emacs-Lisp -*-

(add-to-load-path (home-dir/ "compiled/site-lisp")
                  (home-dir/ "elder"))

(setq-platform git-load-path
               ((darwin . (home-dir/ "git-mode"))
                (windows-nt . "C:/Program Files/Git/contrib")
		(gnu/linux . "")))


(in-platform fry.l2f.inesc-id.pt
  (add-to-load-path "/usr/share/xemacs/xemacs-packages/lisp/text-modes"))

(in-platform darwin
  (add-to-exec-path "/opt/local/bin"
                    "/opt/local/sbin"
                    "/Applications/Dev/Graphviz.app/Contents/MacOS"
                    (home-dir/ "valgrind/bin")
                    (home-dir/ "toolchains/bin")
                    (home-dir/ ".cabal/bin")
                    (home-dir/ "dev/stuff/llvm/llvm/tools/clang/utils")
                    (home-dir/ "llvm/bin"))

  (add-to-load-path ;;(home-dir/ "auctex")
;;                    (home-dir/ "color-theme")
                    (home-dir/ "pov-mode-3.2")
                    git-load-path
;;                    (home-dir/ "jde/lisp")
                    (home-dir/ "haskell-mode")
                    "~/dev/slime"
;;                    (home-dir/ "emms/lisp")
))

(autoload 'graphviz-dot-mode "graphviz-dot-mode")
(setq auto-mode-alist
      (append
       '(("\\.dot\\'" . graphviz-dot-mode))
       auto-mode-alist))


(autoload 'git-status "git" "GIT mode" t)

;; autoloads utility functions
(defun load-def-symbol (load-def)
  "Gets the load-def's symbol"
  (if (listp load-def)
      (nth 0 load-def)
    load-def))
(defun load-def-description (load-def description)
  "Gets the load-def's description"
  (if (and (listp load-def) (>= (length load-def) 2))
      (nth 1 load-def)
    description))
(defun load-def-file (load-def file)
  "Gets the load-def's file"
  (if (and (listp load-def) (>= (length load-def) 3))
      (nth 2 load-def)
    file))
(defun load-def-interactivep (load-def interactivep)
  "Gets the load-def's interactivep setting"
  (if (and (listp load-def) (>= (length load-def) 4))
      (nth 3 load-def)
    interactivep))

(defun autoloads (load-defs file description interactive)
  "Sets up autoloads"
  (mapcar (lambda (load-def)
	    (autoload
	      (load-def-symbol load-def)
	      (load-def-file load-def file)
	      (load-def-description load-def description)
	      (load-def-interactivep interactive interactive)))
	  load-defs))

;; Autoload lisp stuff
(defvar lisp-autoloads
  '(filcab-lisp-mode-hook
    slime lisp-mode
    allegro cmucl openmcl sbcl
    (slime-mode    "Slime mode.")
    (slime-connect "Connect to swank server.")))
(autoloads lisp-autoloads "lisp-stuff" "Slime REPL." t)
(setq auto-mode-alist
      (append
       '(("\\.lisp\\'" . lisp-mode))
       auto-mode-alist))

;; Autoload scheme stuff
(autoload 'scheme-mode "scheme-stuff" "Quack" t)
(setq auto-mode-alist
	    (append
	     '(("\\.ss\\'" . scheme-mode)
	       ("\\.scm\\'" . scheme-mode)
	       ("\\.sch\\'" . scheme-mode))
	     auto-mode-alist))

(autoload 'math "math" "Starts Mathematica" t)
(autoload 'math-mode "math" 
  "Mode for editing Mathematica.  Loading will result in more info." t)
(setq auto-mode-alist (append '(("\\.m\\'" . math-mode)
                                ("\\.nb\\'" . math-mode))
                                auto-mode-alist))


;; Autoload haskell stuff
(defvar haskell-autoloads
  '(haskell-mode turn-on-haskell-ghci))
(autoloads haskell-autoloads "haskell-site-file" "Haskell mode." t)

(setq auto-mode-alist
      (append
       '(("\\.hs\\'" . haskell-mode)
	 ("\\.lhs\\'" . haskell-mode))
       auto-mode-alist))

;; Java hook
(add-hook 'java-mode-hook
	  '(lambda ()
	     (require 'jde)))

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(autoload 'vrml-mode "vrml-mode")
(setq auto-mode-alist
      (append
       '(("\\.wrl\\'" . vrml-mode))
       auto-mode-alist))
