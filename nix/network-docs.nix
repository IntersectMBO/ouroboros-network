inputs: final: prev: {
  network-docs = final.stdenvNoCC.mkDerivation {
    name = "network-docs";
    src = ../.;
    nativeBuildInputs = [
      (final.texlive.combine {
        inherit (final.texlive)
          collection-latexextra
          collection-latexrecommended
          collection-mathscience
          latexmk;
      })
    ];
    buildPhase =
      let
        src = ../.;
        cddl-specs = ../ouroboros-network-protocols/test-cddl/specs;
      in
      ''
        for d in network-design network-spec; do
          mkdir -p docs/$d
          ln -s ${src}/$d/* docs/$d/
        done

        mkdir -p ouroboros-network-protocols/test-cddl/specs
        cp ${cddl-specs}/*.cddl ouroboros-network-protocols/test-cddl/specs

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

        mkdir -p $out/nix-support

        for pdf in $out/*.pdf; do
          echo "file binary-dist $pdf" >> $out/nix-support/hydra-build-products
        done
      '';
  };
}
