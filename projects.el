;;; projects.el --- Project definitions for semantic, EDE, and stuff...

;; Copyright (C) 2010  Filipe Cabecinhas

;; Author: Filipe Cabecinhas <filcab@farnsworth.lan>

(ede-cpp-root-project "LLVM"
                :name "LLVM Project"
                :file "~/dev/stuff/llvm/llvm/ModuleInfo.txt"
                :include-path '("/"
                                "/include"
                                "/tools/clang/include"
                                ;; Should we get the other (private) includes?
                               ))
;;                :system-include-path '("~/...")
;;                :spp-table '(("isUnix" . "")
;;                             ("BOOST_TEST_DYN_LINK" . "")))

(ede-cpp-root-project "clang"
                :name "clang Project"
                :file "~/dev/stuff/llvm/llvm/tools/clang/ModuleInfo.txt"
                :include-path '("/"
                                "/include"
                                ;; Should we get the other (private) includes?
                               ))


(ede-cpp-root-project "Valgrind"
                :name "Valgrind Project"
                :file "~/dev/stuff/valgrind/valgrind10.6/vg-in-place"
                :include-path '("/"
                                "/VEX" "/VEX/priv" "/VEX/pub"
                                "/coregrind"
                                "/include" "/include/vki"
                                "/memcheck"
                                ;; Should we get the other (private) includes?
                               ))

(ede-cpp-root-project "Emacs (trunk)"
                :name "Emacs Project - trunk branch"
                :file "~/dev/stuff/emacs-stuff/emacs-bzr/trunk/BUGS"
                :include-path '("/"
                                "/lib-src" "/lwlib"
                                "/src" "/src/m" "/src/s"
                                ;; Should we get the other (private) includes?
                               ))

(ede-cpp-root-project "Emacs (mac-fullscreen)"
                :name "Emacs Project - mac-fullscreen branch"
                :file "~/dev/stuff/emacs-stuff/emacs-bzr/mac-fullscreen/BUGS"
                :include-path '("/"
                                "/lib-src" "/lwlib"
                                "/src" "/src/m" "/src/s"
                                ;; Should we get the other (private) includes?
                               ))

(provide 'projects)
;;; projects.el ends here
