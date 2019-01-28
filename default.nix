{ compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs     = import ./nix/nixpkgs.nix {};
  lib         = nixpkgs.haskell.lib;
  callCabal2nix = nixpkgs.haskell.packages.${compiler}.callCabal2nix;
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
  docNoSeprateOutput = drv: lib.overrideCabal drv (drv: { enableSeparateDocOutput = false; });
  cleanSource = drv: lib.overrideCabal drv (drv: {src = nixpkgs.lib.sourceFilesBySuffices drv.src [".hs" "LICENSE" "ChangeLog.md" ".cabal"];});

  iohk-monitoring-src = builtins.fetchGit {
    url = "https://github.com/input-output-hk/iohk-monitoring-framework.git";
    rev = "721e672973ecade1e43fbf97c5f2f173d4fb3020";
  };
  iohk-monitoring = docNoSeprateOutput(doHaddock(doTest(doBench(
    callPackage (iohk-monitoring-src + /iohk-monitoring.nix) {}
  ))));

  io-sim-classes = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "io-sim-classes"./io-sim-classes {})
  ))));

  io-sim = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "io-sim" ./io-sim { inherit  io-sim-classes; })
  ))));

  typed-protocols = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "typed-protocols" ./typed-protocols { inherit io-sim-classes io-sim; })
  ))));

  typed-transitions = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "typed-transitions" ./typed-transitions {})
  ))));

  ouroboros-network = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "ouroboros-network" ./ouroboros-network { inherit io-sim io-sim-classes typed-transitions iohk-monitoring; })
  ))));

  ouroboros-consensus = docNoSeprateOutput(doHaddock(doTest(doBench(
    cleanSource (callCabal2nix "ouroboros-consensus" ./ouroboros-consensus { inherit io-sim-classes io-sim typed-transitions ouroboros-network; })
  ))));

in { inherit io-sim-classes io-sim typed-protocols typed-transitions ouroboros-network ouroboros-consensus; }
