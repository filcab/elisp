#EMACS = emacs
export EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs

C := ${PWD}/compiled
EL_PREFIX = $C
EL_BINDIR = $C/bin
EL_SITE = $C/site-lisp
EL_INFO = $C/info
ELISP_CONFIGURE = ./configure --with-emacs=${EMACS} --prefix=${EL_PREFIX} --bindir=${EL_BINDIR}

BUILD_TARGETS = build_auctex build_cedet build_dvc build_ecb \
                build_haskell-mode build_magit build_mo-git-blame

.PHONY: $(BUILD_TARGETS)

all: $(BUILD_TARGETS)

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


