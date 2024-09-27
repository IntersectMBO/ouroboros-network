inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2024-07-04T00:00:00Z";
  tool = name: version: other:
    final.haskell-nix.tool final.ouroboros-network.args.compiler-nix-name name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;
  cabal = tool "cabal" "3.12.1.0" { };
  stylish-haskell = tool "stylish-haskell" "0.14.6.0" { };
  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit (final.ouroboros-network.args) compiler-nix-name;
    index-state = tool-index-state;
  };
}

