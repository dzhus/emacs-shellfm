EMACS := /usr/bin/emacs

compile: shellfm.el shellfm-functions.el
	$(EMACS) --batch -f byte-compile-file $?

clean: FORCE
	rm $(shell hg stat -un)

FORCE: