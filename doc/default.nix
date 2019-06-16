{
  commonLib ? import ../nix/iohk-common.nix,
  pkgs ? commonLib.pkgs,
  stdenv ? pkgs.stdenv,
  texlive ? pkgs.texlive,
}:

let
  tex = texlive.combine {
    # more than we need at the moment, but doesn't cost much to include it
    inherit (texlive)
    etoolbox
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
  document = isRelease: stdenv.mkDerivation {
    name = if isRelease then "network-pdf" else "network-pdf-wip";
    buildInputs = [ tex pkgs.cddl ];
    # this is a hack :
    # network.tex includes ../ouroboros-network/test/messages.cddl .
    # Therefor the src also includes the parrent directory.
    src = pkgs.lib.sourceFilesBySuffices ../. [ ".tex" ".bib" ".pdf" ".cddl"];
    buildPhase = ''
       # run cddl to catch syntax errors in messages.cddl
       cddl ouroboros-network/test/messages.cddl generate 1
       cd doc
       ${if isRelease then "echo >.isRelease" else "rm -f .isRelease"}
       latexmk -view=pdf network;
    '';
    installPhase = ''
      install -Dt $out network.pdf
      mkdir $out/nix-support
      echo "doc-pdf network $out/network.pdf" >> $out/nix-support/hydra-build-products
    '';
  };
in
{
  network-pdf = document true;
  network-pdf-wip = document false;
}
