DOCNAME = network-spec

.PHONY: $(DOCNAME).pdf all clean pvc

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).tex
	latexmk -bibtex -pdf $(DOCNAME).tex

pvc:
	latexmk -pvc -pdf -bibtex $(DOCNAME).tex

clean:
	latexmk -CA
