# Local Dependencies

# This file should be used to declare the main dependencies of
# the project, whereas the main =Makefile= of the project
# is for main rules.


# [[file:README.org::*Local Dependencies][Local Dependencies:1]]
-include deps.mk
# Local Dependencies:1 ends here

# Local configuration

# The =config.mk= file lets you override variables at build time
# for your project


# [[file:README.org::*Local configuration][Local configuration:1]]
-include config.mk
# Local configuration:1 ends here

# Emacs
# First of all we need to define the main emacs


# [[file:README.org::*Emacs][Emacs:1]]
EMACS_CONFIG ?= config.el
EMACS ?= emacs --batch -Q --load $(EMACS_CONFIG)
# Emacs:1 ends here



# and we need org to tex


# [[file:README.org::*Emacs][Emacs:2]]
.PRECIOUS:
%.tangle.tex: %.org
	$(EMACS) $< -f org-babel-tangle

.PRECIOUS:
%.export.tex: %.org
	$(EMACS) $< -f org-latex-export-to-latex

.PRECIOUS:
%.export.html: %.org
	$(EMACS) $< -f org-html-export-to-html

.PRECIOUS:
%.export.beamer.tex: %.org
	$(EMACS) $< -f org-beamer-export-to-latex
# Emacs:2 ends here

# [[file:README.org::*Tectonic latex][Tectonic latex:2]]
TECTONIC_RERUNS ?=
%.pdf: %.tex
	$(info [TEX] $< -> $@)
	tectonic $(TECTONIC_RERUNS) $<
# Tectonic latex:2 ends here
