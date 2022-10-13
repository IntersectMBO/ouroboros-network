{ sources }:
# our packages overlay
pkgs: _:
with pkgs; {
  ouroborosNetworkHaskellPackages = import ./ouroboros-network.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
    inherit (sources) CHaP;
  };

  ouroborosNetworkHaskellPackagesWithTVarCheck = import ./ouroboros-network.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
    checkTVarInvariant = true;
    inherit (sources) CHaP;
  };

  network-docs = callPackage ./network-docs.nix { };
  consensus-docs = callPackage ./consensus-docs.nix { };

  cabal =
    haskell-nix.tool localConfig.ghcVersion "cabal" { version = "latest"; };

  # can be changed back to haskell-nix.tool when we bump our index-state
  stylish-haskell = (haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      owner = "haskell";
      repo = "stylish-haskell";
      rev = "v0.14.3.0";
      sha256 = "sha256-TF8VxrkE142D6dhWMbuAWTfVTafm8I2kpSnyW4eA8d0=";
    };
    compiler-nix-name = localConfig.ghcVersion;
    inherit (ouroborosNetworkHaskellPackages) index-state;
  }).stylish-haskell.components.exes.stylish-haskell;

  trace = builtins.trace;
}
