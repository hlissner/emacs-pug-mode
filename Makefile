
all:
	@cask

test:
	@cask exec ert-runner -l pug-mode.el

clean:
	@rm -rf .cask
	@rm -f *.elc test/*.elc

.PHONY: test
