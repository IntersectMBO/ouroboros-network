# Builds using system <nixpkgs> and ghc863 with the required package curation.
let
  nixpkgs = import <nixpkgs> {};

  overrides = import ./nix/overrides.nix { inherit nixpkgs; };

  cardanoroot = ../cardano-sl;
  cardanopkgs = import ./nix/cardanopkgs.nix { inherit nixpkgs; inherit cardanoroot; };

  # Build on 8.6.3 with the required overrides: one for packages defined in
  # this repo, and one to bring in cardano-sl as a library, which is a real
  # difficult thing to do apparently.
  compiler = nixpkgs.haskell.packages.ghc863.override {
    overrides = self: super:
      (overrides self super) // (cardanopkgs self super);
  };
in
  import ./pkgs.nix { inherit nixpkgs; inherit compiler; }
