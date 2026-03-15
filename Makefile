EMACS ?= emacs
ELISP_SOURCES := $(shell find lisp -type f -name '*.el' | sort)

.PHONY: help run load-save dump-save compile clean

help:
	@printf "ElCity Make targets\n"
	@printf "  make run                  Start game entrypoint\n"
	@printf "  make load-save FILE=...   Load save file and print summary (CLI)\n"
	@printf "  make dump-save FILE=...   Load save file and print machine-readable dump\n"
	@printf "  make compile              Byte-compile all Elisp sources\n"
	@printf "  make clean                Remove compiled files\n"

run:
	$(EMACS) -Q -L lisp -l lisp/elcity-player.el -f elcity-player-start

load-save:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make load-save FILE=path/to/save.elcity"; \
		exit 1; \
	fi
	$(EMACS) -Q --batch -L lisp -l lisp/elcity-persist.el -f elcity-persist-batch-load "$(FILE)"

dump-save:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make dump-save FILE=path/to/save.elcity"; \
		exit 1; \
	fi
	$(EMACS) -Q --batch -L lisp -l lisp/elcity-persist.el -f elcity-persist-batch-dump "$(FILE)"

compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile $(ELISP_SOURCES)

clean:
	find . -type f \( -name "*.elc" -o -name "*.eln" \) -delete
