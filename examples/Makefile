.PHONY: all clean

all: regexp

regexp: regexp.ml
	ocamlfind ocamlc -linkpkg -syntax camlp4o -package text.pcre -o regexp regexp.ml

clean:
	rm -f regexp *.cm* *.o
