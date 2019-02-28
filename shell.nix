{ withHoogle ? false 
, haddock    ? false
, test       ? false
, benchmarks ? false
, error      ? false
}:
let
  pkgs  = import ./. { inherit haddock test benchmarks error; };
in
with builtins; with pkgs.compiler;
  shellFor {
    packages    = p: attrValues (removeAttrs pkgs ["compiler" "nixpkgs"]);
    withHoogle  = withHoogle;
    buildInputs = [ cabal-install ];
  }
