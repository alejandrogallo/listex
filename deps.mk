DEPS = \
listex.el t.el \

all: $(DEPS) test

test: listex.el t.el
	$(EMACS) --load listex.el --load t.el

listex.el t.el: README.org
	$(EMACS) $< -f org-babel-tangle

.PHONY: all test
