EMACS := /usr/bin/emacs

recompile: shellfm.el
	$(EMACS) --batch -q -l shellfm.el \
	--eval "(batch-byte-recompile-directory 0)" .

clean: FORCE
	rm $(shell hg stat -un)

FORCE: