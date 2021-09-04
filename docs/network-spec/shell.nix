# environment to build network-spec doc pdf
with (import <nixpkgs> {});
let
 localTeX = pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-basic latexmk

          collection-fontsrecommended

          cleveref
          etoolbox
          float
          framed
          lazylist
          listings
          mathtools
          microtype
          polytable
          stmaryrd
          todonotes
          xcolor
        ;
      };

in
mkShell {
  buildInputs = [
    haskellPackages.lhs2tex
    localTeX
  ];
}
