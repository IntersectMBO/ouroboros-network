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

  # Special cardano branch for iohk montiroing framework compat
  cardanoroot = nixpkgs.fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl";
    rev = "869f83e939077e492085a2a2b8aeebb558255b00";
    sha256 = "1yf2qz394ax223mqnhy4aqk165c2j76jn4bnmpdps22sq9ckcap5";
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
