EMACS=emacs

elc:
	$(EMACS) -batch -l sqlquery.el -l vc-svn-21.el -l dsvn.el -f batch-byte-compile *.el

clean:
	$(RM) *.elc
