{ pkgs }: with pkgs;
let
  src = haskell-nix.haskellLib.cleanGit {
      name = "ouroboros-network-docs-src";
      src = ../.;
      subDir = "docs";
  };
  message-cddl = ../ouroboros-network/test/messages.cddl;
in pkgs.runCommand "ouroboros-network-docs" {
  nativeBuildInputs = [ imagemagick ];
  buildInputs = [ (texlive.combine {
    inherit (texlive)
      cleveref
      framed
      scheme-small
      collection-fontsrecommended
      stmaryrd kpfonts geometry hyperref
      todonotes
      amsmath mathtools
      colortbl polytable lazylist
      fancyvrb
      #graphicx
      pstricks
      wrapfig
      # build tools
      latexmk
    ;
  })];
} ''
  for d in network-design network-spec connection-manager-spec; do
    mkdir -p docs/$d
    ln -s ${src}/$d/* docs/$d/
  done

  mkdir -p ouroboros-network/test
  cp ${message-cddl} ouroboros-network/test/messages.cddl

  mkdir -p $out

  (
    cd docs/network-design
    latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode"
    cp -a *.pdf $out/
  )

  (
    cd docs/network-spec
    make all
    cp -a *.pdf $out/
  )

  (
    cd docs/connection-manager-spec
    latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode"
    cp -a *.pdf $out/
  )

  mkdir -p $out/nix-support

  for pdf in $out/*.pdf; do
    echo "file binary-dist $pdf" >> $out/nix-support/hydra-build-products
  done
''
