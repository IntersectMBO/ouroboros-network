{ compiler   ? "ghc862"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs     = import ./nix/nixpkgs.nix {};
  lib         = nixpkgs.haskell.lib;
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
    callPackage ./typed-transitions/default.nix {}
  )));

  ouroboros-network = doHaddock(doTest(doBench(
    callPackage ./ouroboros-network/default.nix { inherit typed-transitions; }
  )));

  ouroboros-consensus = doHaddock(doTest(doBench(
    callPackage ./ouroboros-consensus/default.nix { inherit nixpkgs typed-transitions ouroboros-network; }
  )));

in { inherit typed-transitions ouroboros-network ouroboros-consensus; }
