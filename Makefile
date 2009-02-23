# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of encoding.

ifeq ($(TERM),dumb)
OC = ocamlbuild -classic-display
else
OC = ocamlbuild
endif
OF = ocamlfind

NAME = encoding
PUBLIC = encoding encoding_table enc_string enc_utf8

.PHONY: all
all:
	$(OC) META $(NAME).cma $(NAME).cmxa

.PHONY: examples
examples:
	$(OC) examples/iconv.native examples/iconv_lwt.native

.PHONY: doc
doc:
	$(OC) $(NAME).docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(NAME)-`head -n 1 VERSION`

.PHONY: install
install:
	$(OF) install $(NAME) _build/META \
	  ${PUBLIC:%=_build/src/%.mli} \
	  ${PUBLIC:%=_build/src/%.cmi} \
	  _build/src/*.cmx \
	  _build/$(NAME).cma \
	  _build/$(NAME).cmxa

.PHONY: uninstall
uninstall:
	$(OF) remove $(NAME)

.PHONY: clean
clean:
	$(OC) -clean
