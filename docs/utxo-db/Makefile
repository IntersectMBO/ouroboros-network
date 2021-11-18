
all: utxo-db.pdf utxo-db-api.pdf

%.pdf: %.tex
	latexmk -bibtex -pdf $<

%.tex: %.lhs
	lhs2TeX -o $@ $<

clean:
	latexmk -CA
	rm -f utxo-db-api.tex

utxo-db-api.pdf: hstore-anticaching-fig1.pdf
