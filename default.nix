{ haddock    ? true
, test       ? true
, benchmarks ? false
, error      ? false
}:
# Builds using system <nixpkgs> and ghc863 with the required package curation.
let
  # Pin nixpkgs to this one.
  nixpkgs = import ./nix/nixpkgs.nix {};
  overrides = import ./nix/overrides.nix { inherit nixpkgs; };

  cardanoroot = nixpkgs.fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl";
    rev = "5a4eee1ebded257d785d39a090a216c4a261eb22";
    sha256 = "0wr3vn7vdc455dfh57aslqr59fx1vgmi6xwdrrfjh8gg00q1dkng";
  };
  cardanopkgs = import ./nix/cardanopkgs.nix { inherit nixpkgs cardanoroot; };

  # Build on 8.6.3 with the required overrides: one for packages defined in
  # this repo, and one to bring in cardano-sl as a library, which is a real
  # difficult thing to do apparently.
  compiler = nixpkgs.haskell.packages.ghc863.override {
    overrides = self: super:
      (overrides self super) // (cardanopkgs self super);
  };
in
  (import ./pkgs.nix { inherit nixpkgs compiler haddock test benchmarks error; }) // {
    inherit nixpkgs compiler;
  }
