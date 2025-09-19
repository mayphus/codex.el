EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -L . -l test/run-tests.el
