.PHONY: all clean

all: docs.pdf

specs: specifications.tex
	pdflatex specifications.tex

docs.pdf: docs.tex
	pdflatex docs.tex

clean:
	@rm -f *.out *.log *.bbl *.aux

