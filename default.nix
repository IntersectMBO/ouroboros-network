{ compiler   ? "ghc844"
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

  iohk-monitoring-src = builtins.fetchGit {
    url = "https://github.com/input-output-hk/iohk-monitoring-framework.git";
    rev = "721e672973ecade1e43fbf97c5f2f173d4fb3020";
  };
  iohk-monitoring = doHaddock(doTest(doBench(
    callPackage (iohk-monitoring-src + /iohk-monitoring.nix) {}
  )));

  io-sim-classes = doHaddock(doTest(doBench(
    callPackage ./io-sim-classes/default.nix { inherit nixpkgs; }
  )));

  io-sim = doHaddock(doTest(doBench(
    callPackage ./io-sim/default.nix { inherit  nixpkgs io-sim-classes; }
  )));

  typed-transitions = doHaddock(doTest(doBench(
    callPackage ./typed-transitions/default.nix { inherit nixpkgs; } 
  )));

  ouroboros-network = doHaddock(doTest(doBench(
    callPackage ./ouroboros-network/default.nix { inherit nixpkgs io-sim io-sim-classes typed-transitions iohk-monitoring; }
  )));

  ouroboros-consensus = doHaddock(doTest(doBench(
    callPackage ./ouroboros-consensus/default.nix { inherit nixpkgs io-sim-classes typed-transitions ouroboros-network; }
  )));

in { inherit io-sim-classes io-sim typed-transitions ouroboros-network ouroboros-consensus; }
