{ sources }:
# our packages overlay
pkgs: _:
with pkgs;
let
  tool = name: version: other:
    haskell-nix.tool localConfig.ghcVersion name ({
      version = version;
      index-state = localConfig.tools-index-state;
    } // other);
in {
  ouroborosNetworkHaskellPackages = import ./ouroboros-network.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
    inherit (sources) CHaP;
  };

  ouroborosNetworkHaskellPackagesWithTVarCheck =
    import ./ouroboros-network.nix {
      inherit config pkgs lib stdenv haskell-nix buildPackages;
      checkTVarInvariants = true;
      inherit (sources) CHaP;
    };

  network-docs = callPackage ./network-docs.nix { };

  cabal = tool "cabal" "latest" { };

  stylish-haskell = tool "stylish-haskell" "0.14.4.0" {
    cabalProjectLocal = "allow-older: ghc-lib-parser:base";
  };

  cabal-fmt = tool "cabal-fmt" "latest" { };

  scriv = pkgs.callPackage ./scriv.nix { };

  trace = builtins.trace;
}
