inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2026-02-17T10:15:41Z";
  tool = name: version: other:
    final.haskell-nix.tool final.ouroboros-network.args.compiler-nix-name name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;
  cabal = tool "cabal" "3.12.1.0" { };
  cabal-gild = tool "cabal-gild" "1.5.0.1" { };
  stylish-haskell = tool "stylish-haskell" "0.14.6.0" { };
  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit (final.ouroboros-network.args) compiler-nix-name;
    index-state = tool-index-state;
  };
}
