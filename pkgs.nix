{ nixpkgs
, compiler
, haddock    ? false
, test       ? false
, benchmarks ? false
}:
with builtins;
let
  lib           = nixpkgs.haskell.lib;
  callCabal2nix = compiler.callCabal2nix;
  callPackage   = compiler.callPackage;

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

  pkgs = rec {

    io-sim-classes = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "io-sim-classes"./io-sim-classes {})
    ))));

    io-sim = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "io-sim" ./io-sim { inherit io-sim-classes; })
    ))));

    typed-transitions = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "typed-transitions" ./typed-transitions {})
    ))));

    ouroboros-network = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "ouroboros-network" ./ouroboros-network { inherit io-sim io-sim-classes typed-transitions; })
    ))));

    ouroboros-consensus = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "ouroboros-consensus" ./ouroboros-consensus { inherit io-sim-classes io-sim typed-transitions ouroboros-network; })
    ))));

  };

in
  pkgs
