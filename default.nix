{ compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  lib = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

  typed-transitions = doHaddock(doTest(doBench(
    callPackage ./typed-transitions/default.nix { inherit nixpkgs; }
  )));

  ouroboros-network = doHaddock(doTest(doBench(
    callPackage ./pkg.nix {
      inherit nixpkgs;
    })))) "test-with-cabal";

in { inherit ouroboros-network typed-transitions; }
