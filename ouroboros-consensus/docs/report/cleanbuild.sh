#!/bin/bash

SOURCES=`find . -name '*.tex'`
MAIN=report.tex

rm -f *.aux *.log *.out *.pdf *.toc *.bbl *.blg *.nav *.snm

pdflatex -halt-on-error $MAIN >/dev/null
bibtex report
pdflatex -halt-on-error $MAIN >/dev/null
pdflatex -halt-on-error $MAIN >pdflatex.log
