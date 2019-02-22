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

  # This is in the develop branch. The oldest commit against which the Byron
  # proxy will build.
  cardanoroot = nixpkgs.fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl";
    rev = "1c8b414f59016c360c0f0eb2ec9cef07c99063ef";
    sha256 = "0dzd9gj3hvcymqwz17widyhxq366yr669pijmvsvys6ghgxc13nk";
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
