EMACS = emacs

C := ${PWD}/compiled
EL_PREFIX = $C
EL_BINDIR = $C/bin
EL_SITE = $C/site-lisp
EL_INFO = $C/info
ELISP_CONFIGURE = ./configure --with-emacs=${EMACS} --prefix=${EL_PREFIX} --bindir=${EL_BINDIR}

.PHONY: build_auctex

build_auctex:
	cd auctex && \
	if [ -f ./autogen.sh ]; then ./autogen.sh; fi && \
	${ELISP_CONFIGURE} --without-texmf-dir \
		--with-lispdir=${EL_SITE} \
		--infodir=${EL_INFO} && \
	make all install


