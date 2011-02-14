.PHONY: all clean clean-aux

all: manual.pdf

%.pdf: %.tex
	rubber --pdf $<

%.html: %.tex
	hevea -fix manual.tex

clean: clean-aux
	rm -f *.pdf

clean-aux:
	rm -f *.aux *.dvi *.log *.out *.toc *.html *.htoc *.haux
