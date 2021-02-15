{ pkgs }: with pkgs;
let
  src = haskell-nix.haskellLib.cleanGit {
      name = "ouroboros-consensus-docs-src";
      src = ../.;
      subDir = "ouroboros-consensus/docs";
  };
in pkgs.runCommand "ouroboros-consensus-docs" {
  nativeBuildInputs = [ bash ];
  buildInputs = [ (texlive.combine {
    inherit (texlive)
      amsmath
      cleveref
      collection-fontsrecommended
      enumitem
      latexmk
      scheme-small
      siunitx
      todonotes
    ;
  })];
} ''

  for d in report; do
    mkdir -p docs/$d
    ln -s ${src}/$d/* docs/$d/
  done

  mkdir -p $out

  (
    cd docs/report
    latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" report.tex
    cp -a *.pdf $out/
  )

  mkdir -p $out/nix-support

  for pdf in $out/*.pdf; do
    echo "file binary-dist $pdf" >> $out/nix-support/hydra-build-products
  done
''
