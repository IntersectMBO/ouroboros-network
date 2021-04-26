##
## Makefile for a specification of the delegation rules, based on:
##
## https://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project
##

# Document name
ifndef DOCNAME
	DOCNAME = network-spec
endif

# You want latexmk to *always* run, because make does not have all the info.
# Also, include non-file targets in .PHONY so they are run regardless of any
# file of the given name existing.
.PHONY: $(DOCNAME).pdf all clean

# The first rule in a Makefile is the one executed by default ("make"). It
# should always be the "all" rule, so that "make" and "make all" are identical.
all: $(DOCNAME).pdf

##
## CUSTOM BUILD RULES
##

##
## MAIN LATEXMK RULE
##

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interaction=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

$(DOCNAME).pdf: $(DOCNAME).tex
	latexmk -bibtex -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make $(DOCNAME).tex

watch: network-spec.tex miniprotocols.tex ../../ouroboros-network/test/messages.cddl
	latexmk -bibtex -pvc -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make $(DOCNAME).tex

clean:
	latexmk -CA

.PHONY: updaterevision
updaterevision:
	git log --follow --oneline ./ |wc -l > ./revisioncount.tex
