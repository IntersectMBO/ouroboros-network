{ compiler   ? "ghc843"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
  lib = nixpkgs.haskell.lib;
  pkgs = nixpkgs.haskell.packages.${compiler};
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

  ouroboros-network = lib.enableCabalFlag (doHaddock(doTest(doBench(
    callPackage ./pkg.nix {
      QuickCheck = pkgs.QuickCheck_2_12_4;
      inherit nixpkgs;
    })))) "test-with-cabal";

in { inherit ouroboros-network; }
