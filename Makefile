CASK ?= cask
EMACS ?= emacs

all: test

test:
	$(CASK) exec ert-runner --verbose --debug

.PHONY: test
