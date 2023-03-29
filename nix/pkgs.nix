{ sources }:
# our packages overlay
pkgs: _:
with pkgs; {
  ouroborosNetworkHaskellPackages = import ./ouroboros-network.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
    inherit (sources) CHaP;
  };

  ouroborosNetworkHaskellPackagesWithTVarCheck =
    import ./ouroboros-network.nix {
      inherit config pkgs lib stdenv haskell-nix buildPackages;
      checkTVarInvariant = true;
      inherit (sources) CHaP;
    };

  network-docs = callPackage ./network-docs.nix { };

  cabal =
    haskell-nix.tool localConfig.ghcVersion "cabal" { version = "latest"; };

  stylish-haskell = haskell-nix.tool localConfig.ghcVersion "stylish-haskell" {
    version = "0.14.4.0";
    cabalProjectLocal = "allow-older: ghc-lib-parser:base";
  };

  cabal-fmt =
    haskell-nix.tool localConfig.ghcVersion "cabal-fmt" { version = "latest"; };

  scriv = pkgs.callPackage ./scriv.nix { };

  trace = builtins.trace;
}
