{ nixpkgs
, compiler
, haddock    ? true
, test       ? true
, benchmarks ? false
, error      ? false
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
  doWerror = if error
    then drv: lib.appendConfigureFlag drv "--ghc-option=-Werror"
    else nixpkgs.lib.id;

  pkgs = rec {

    io-sim-classes = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "io-sim-classes"./io-sim-classes {})
    )))));

    io-sim = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "io-sim" ./io-sim { inherit io-sim-classes; })
    )))));

    typed-protocols = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "typed-protocols" ./typed-protocols { inherit io-sim io-sim-classes; })
    )))));

    typed-transitions = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "typed-transitions" ./typed-transitions {})
    )))));

    ouroboros-network = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "ouroboros-network" ./ouroboros-network { inherit io-sim io-sim-classes typed-protocols typed-transitions; })
    )))));

    ouroboros-consensus = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "ouroboros-consensus" ./ouroboros-consensus { inherit io-sim-classes io-sim typed-protocols typed-transitions ouroboros-network; })
    )))));

    byron-proxy = doWerror(docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "byron-proxy" ./byron-proxy { inherit ouroboros-network; inherit ouroboros-consensus; inherit io-sim; inherit io-sim-classes; })
    )))));

  };

in
  pkgs
