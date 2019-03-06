{ haddock    ? true
, test       ? true
, benchmarks ? false
, error      ? false
, profiling  ? false
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
    rev = "d09d265e6f78621b2520f4b2b98d177c8695c138";
    sha256 = "0dr75q5gqyhwvbb53rpq7lsjxp7nm97rkd4l78vdi9br49hhrbch";
  };
  cardanopkgs = import ./nix/cardanopkgs.nix { inherit nixpkgs cardanoroot; };

  setProfiling = super: {
    mkDerivation = args: super.mkDerivation (args // {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
      /* Profiling makes a bunch of template haskell stuff fail in tests */
      doCheck = false;
    });
  };

  # Build on 8.6.3 with the required overrides: one for packages defined in
  # this repo, and one to bring in cardano-sl as a library, which is a real
  # difficult thing to do apparently.
  compiler = nixpkgs.haskell.packages.ghc863.override {
    overrides = self: super:
      (overrides self super) // (cardanopkgs self super) //
      (if profiling then setProfiling super else {});
  };
in
  (import ./pkgs.nix { inherit nixpkgs compiler haddock test benchmarks error; }) // {
    inherit nixpkgs compiler;
  }
