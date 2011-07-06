ifeq ($(shell hostname), fry)
	export EMACS := emacs
else
#	ifeq ($(shell hostname), farnsworth)
	export EMACS := /Applications/Emacs.app/Contents/MacOS/Emacs
endif

C := ${PWD}/compiled
EL_PREFIX = $C
EL_BINDIR = $C/bin
EL_SITE = $C/site-lisp
EL_INFO = $C/info
ELISP_CONFIGURE = ./configure --with-emacs=${EMACS} --prefix=${EL_PREFIX} --bindir=${EL_BINDIR}

BUILD_TARGETS = build_auctex build_cedet build_dvc build_ecb \
                build_haskell-mode build_magit build_mo-git-blame \
                build_markdown-mode build_jd-el build_matlab

.PHONY: $(BUILD_TARGETS) which all

all: which $(BUILD_TARGETS)

which:
	@echo Using Emacs command \`$(EMACS)\'

build_auctex:
	@cd auctex && \
	if [ -f ./autogen.sh ]; then ./autogen.sh; fi && \
	${ELISP_CONFIGURE} --without-texmf-dir \
		--with-lispdir=${EL_SITE} \
		--infodir=${EL_INFO} && \
	make all install

build_cedet:
	@cd cedet && \
	touch `find . -name Makefile` && \
	make EMACS=$(EMACS) PREFIX=$(EL_PREFIX)

build_dvc:
	@cd dvc && \
	if [ ! -f configure ]; then autoconf; fi && \
	${ELISP_CONFIGURE} && time make && make install

build_ecb:
	@cd ecb && \
	make EMACS=$(EMACS) CEDET="$(PWD)/cedet" LOADPATH=`pwd` && \
	$(EMACS) -batch -no-site-file \
		 --eval "(add-to-list 'load-path \"`pwd`\")" \
		 -l ecb-autogen -f ecb-update-autoloads

build_haskell-mode:
	@${EMACS} --eval "(add-to-list 'load-path \"${PWD}/haskell-mode\")" \
		  --eval '(byte-recompile-directory "${PWD}/haskell-mode" 0)' \
		  --batch

build_magit:
	@cd magit && \
	./autogen.sh && \
	${ELISP_CONFIGURE} && \
	time make install

build_mo-git-blame:
	@${EMACS} --eval "(add-to-list 'load-path \"${PWD}/mo-git-blame\")" \
	--eval '(byte-recompile-directory "${PWD}/mo-git-blame" 0)' \
	--batch

build_markdown-mode:
	@${EMACS} --eval "(add-to-list 'load-path \"${PWD}/markdown-mode\")" \
	--eval '(byte-recompile-directory "${PWD}/markdown-mode" 0)' \
	--batch

build_jd-el:
	@${EMACS} --eval "(add-to-list 'load-path \"${PWD}/jd-el\")" \
	--eval '(byte-recompile-directory "${PWD}/jd-el" 0)' \
	--batch

build_matlab:
	@${EMACS} --eval "(add-to-list 'load-path \"${PWD}/matlab-emacs\")" \
	--eval '(byte-recompile-directory "${PWD}/matlab-emacs" 0)' \
	--batch

