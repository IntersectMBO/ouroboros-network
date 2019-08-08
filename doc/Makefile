##
## Makefile for a specification of the delegation rules, based on:
##
## https://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project
##

# You want latexmk to *always* run, because make does not have all the info.
# Also, include non-file targets in .PHONY so they are run regardless of any
# file of the given name existing.
.PHONY: network.pdf all clean

# The first rule in a Makefile is the one executed by default ("make"). It
# should always be the "all" rule, so that "make" and "make all" are identical.
all: network.pdf

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

network.pdf: network.tex intro.tex part2.tex messages.cddl.incl
	latexmk -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make network.tex

watch: network.tex intro.tex part2.tex messages.cddl.incl
	latexmk -pvc -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make network.tex

clean:
	latexmk -CA
	rm -f messages.cddl.incl


messages.cddl.incl:
# Make the cddl-spec visible to for latex.
# When make is called from nix ../ is not visible and this will create a stale link
# nix must take care of creating messages.cddl.incl beforehand.
	ln -s ../ouroboros-network/test/messages.cddl messages.cddl.incl


.PHONY: updaterevision
updaterevision:
	git log --follow --oneline ./ |wc -l > ./revisioncount
