{ withHoogle ? false
}:
let
  pkgs  = import ./.;
in
with builtins; with pkgs.compiler;
  shellFor {
    packages    = p: attrValues (removeAttrs pkgs ["compiler" "nixpkgs"]);
    withHoogle  = withHoogle;
    buildInputs = [ cabal-install ];
  }
