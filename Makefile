EMACS ?= emacs

FILES = lispm-eseq.el lispm-eseq-lisp.el lispm-eseq-ui.el

.PHONY: test compile

# The tests stand in for primitives such as call-process; without native
# trampolines for them, doing so starts no compiler.
test:
	$(EMACS) -Q --batch -L . --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-eseq-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
