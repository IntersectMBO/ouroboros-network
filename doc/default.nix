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
    IEEEtran
    acmart
    bibtex
    biblatex
    latexmk;
  };
  cddl-spec = stdenv.mkDerivation {
    name= "messages.cddl";
    buildInputs = [ pkgs.cddl ];
    src = pkgs.lib.sourceFilesBySuffices ../ouroboros-network/test [".cddl"];
    buildPhase = ''
      cddl messages.cddl generate 1
    '';
    installPhase = ''
      install -Dt $out messages.cddl
    '';
  };

  document = isRelease: stdenv.mkDerivation {
    name = if isRelease then "network-pdf" else "network-pdf-wip";
    buildInputs = [ tex pkgs.cddl ];
    src = ./. ;
    buildPhase = ''
      rm -f messages.cddl.incl
      cp ${cddl-spec}/messages.cddl messages.cddl.incl
      ${if isRelease then "echo >.isRelease" else "rm -f .isRelease"}
      make network.pdf
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
  inherit cddl-spec;
}
