.PHONY: test clean

EMACS ?= emacs

# Detect straight.el build directory (Doom Emacs)
EMACS_VERSION := $(shell $(EMACS) --batch --eval '(princ emacs-version)' 2>/dev/null)
STRAIGHT_DIR ?= $(HOME)/.emacs.d/.local/straight/build-$(EMACS_VERSION)

# If cask is available, use it; otherwise use straight.el load paths
CASK := $(shell command -v cask 2>/dev/null)

ifdef CASK
  EMACS_CMD = $(CASK) exec $(EMACS)
  CASK_LOAD =
else ifneq ($(wildcard $(STRAIGHT_DIR)/buttercup),)
  EMACS_CMD = $(EMACS)
  CASK_LOAD = -L $(STRAIGHT_DIR)/buttercup \
	-L $(STRAIGHT_DIR)/yaml \
	-L $(STRAIGHT_DIR)/treemacs \
	-L $(STRAIGHT_DIR)/cfrs \
	-L $(STRAIGHT_DIR)/pfuture \
	-L $(STRAIGHT_DIR)/ht \
	-L $(STRAIGHT_DIR)/s \
	-L $(STRAIGHT_DIR)/dash \
	-L $(STRAIGHT_DIR)/ace-window \
	-L $(STRAIGHT_DIR)/avy \
	-L $(STRAIGHT_DIR)/hydra \
	-L $(STRAIGHT_DIR)/posframe \
	-L $(STRAIGHT_DIR)/lv
else
  $(error No test runner found. Install Cask or ensure straight.el packages are built)
endif

test:
	$(EMACS_CMD) --batch -L . -L test $(CASK_LOAD) \
		-l buttercup \
		-l test-helper \
		-l test-declarative-project \
		-l test-declarative-project-treemacs \
		-l test-ob-declarative-project \
		-f buttercup-run

clean:
	rm -rf .cask/
