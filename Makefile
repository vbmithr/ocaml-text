# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-text.

OC = ocamlbuild -classic-display
OF = ocamlfind

NAME = text

# Check wether native compilation is available:
ifeq ($(shell $(OF) ocamlopt -version 2> /dev/null),)
  HAVE_NATIVE = false
else
  HAVE_NATIVE = true
endif

# Common targets:
TARGETS = META src/$(NAME).cma

ifeq ($(HAVE_NATIVE),true)
  # Native targets:
  TARGETS += src/$(NAME).cmxa src/$(NAME).cmxs
endif

.PHONY: all
all:
	$(OC) $(TARGETS)

.PHONY: doc
doc:
	$(OC) $(NAME).docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name ocaml-$(NAME)-`head -n 1 VERSION`

.PHONY: install
install:
	$(OF) install $(NAME) _build/META \
	  $(wildcard _build/src/*.mli) \
	  $(wildcard _build/src/*.cmi) \
	  $(wildcard _build/src/*.cmx) \
	  $(wildcard _build/src/*.cma) \
	  $(wildcard _build/src/*.cmxa) \
	  $(wildcard _build/src/*.cmxs) \
	  $(wildcard _build/src/*.so) \
	  $(wildcard _build/src/*.a)

.PHONY: uninstall
uninstall:
	$(OF) remove $(NAME)

.PHONY: reinstall
reinstall: uninstall install

.PHONY: clean
clean:
	$(OC) -clean
