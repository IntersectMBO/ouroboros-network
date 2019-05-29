{
  commonLib ? import ../nix/iohk-common.nix,
  pkgs ? commonLib.pkgs,
  stdenv ? pkgs.stdenv,
  texlive ? pkgs.texlive
}:

let
  tex = texlive.combine {
    # more than we need at the moment, but doesn't cost much to include it
    inherit (texlive)
    scheme-small
    collection-bibtexextra
    collection-latex
    collection-latexextra
    collection-luatex
    collection-fontsextra
    collection-fontsrecommended
    collection-mathscience
    acmart
    bibtex biblatex
    latexmk;
  };
in
stdenv.mkDerivation {
  name = "network-pdf";
  buildInputs = [ tex pkgs.cddl ];
  # this is a hack :
  # network.tex includes ../ouroboros-network/test/messages.cddl .
  # Therefor the src also includes the parrent directory.
  src = pkgs.lib.sourceFilesBySuffices ../. [ ".tex" ".bib" ".pdf" ".cddl"];
  buildPhase = ''
     # generate one output to catch syntax errors in messages.cddl
     cddl ouroboros-network/test/messages.cddl generate 1
     cd doc
     latexmk -view=pdf network;
  '';
  installPhase = "install -Dt $out network.pdf";
}

