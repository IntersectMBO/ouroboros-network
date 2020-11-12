#!/bin/bash

SOURCES=`find . -name '*.tex'`
MAIN=report.tex

while inotifywait $SOURCES
do
  echo "Building.."
  pdflatex -halt-on-error $MAIN >/dev/null
  bibtex report
  pdflatex -halt-on-error $MAIN >/dev/null
  pdflatex -halt-on-error $MAIN >pdflatex.log
  grep "LaTeX Warning:" pdflatex.log
  echo "OK"
done
