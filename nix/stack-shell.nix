with import ./iohk-common.nix;
with pkgs;

let
  stack-pkgs = import ./.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "ouroboros-network-env";
  buildInputs = [ zlib openssl lzma rocksdb git ];
  ghc = haskell.packages.${compiler}.ghc;
}
