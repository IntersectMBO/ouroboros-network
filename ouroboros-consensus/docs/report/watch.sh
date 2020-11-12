#!/bin/bash

SOURCES=`find . -name '*.tex'`

while inotifywait $SOURCES
do
  echo "Building.."
  pdflatex -halt-on-error report.tex >/dev/null
  bibtex report
  pdflatex -halt-on-error report.tex >/dev/null
  pdflatex -halt-on-error report.tex >pdflatex.log
  grep "LaTeX Warning:" pdflatex.log
  echo "OK"
done
