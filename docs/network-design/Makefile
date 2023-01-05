DOCNAME = network-design

.PHONY: $(DOCNAME).pdf all clean

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).tex
	latexmk -bibtex -pdf $(DOCNAME).tex

clean:
	latexmk -CA

