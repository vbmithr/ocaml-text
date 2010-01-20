# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-text.

OC = ocamlbuild -classic-display
OF = ocamlfind

.PHONY: all
all:
	$(OC) all

.PHONY: tests
tests:
	$(OC) test_programs

.PHONY: doc
doc:
	$(OC) text.docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name ocaml-text-`head -n 1 VERSION`

.PHONY: install
install:
	$(OF) install text _build/META \
	  $(wildcard _build/src/*.mli) \
	  $(wildcard _build/src/*.cmi) \
	  $(wildcard _build/src/*.cmx) \
	  $(wildcard _build/src/*.cma) \
	  $(wildcard _build/src/*.cmxa) \
	  $(wildcard _build/src/*.cmxs) \
	  $(wildcard _build/src/*.so) \
	  $(wildcard _build/src/*.a) \
	  $(wildcard _build/syntax/pa_text_pcre.cma)

.PHONY: uninstall
uninstall:
	$(OF) remove text

.PHONY: reinstall
reinstall: uninstall install

.PHONY: clean
clean:
	$(OC) -clean
