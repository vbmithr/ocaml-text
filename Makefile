# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-text.

OC = ocamlbuild -classic-display
OF = ocamlfind

NAME = text

.PHONY: all
all:
	$(OC) META $(NAME).cma $(NAME).cmxa

.PHONY: doc
doc:
	$(OC) $(NAME).docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(NAME)-`head -n 1 VERSION`

.PHONY: install
install:
	$(OF) install $(NAME) _build/META \
	  _build/src/*.mli \
	  _build/src/*.cmi \
	  _build/src/*.cmx \
	  _build/src/*.a \
	  _build/src/*.so \
	  _build/$(NAME).cma \
	  _build/$(NAME).cmxa \
	  _build/$(NAME).a

.PHONY: uninstall
uninstall:
	$(OF) remove $(NAME)

.PHONY: reinstall
reinstall: uninstall install

.PHONY: clean
clean:
	$(OC) -clean
