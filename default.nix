{ nixpkgs    ? import <nixpkgs> {}
, compiler   ? "ghc862"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs     = import ./nix/nixpkgs.nix {};
  lib         = nixpkgs.haskell.lib;

  cardano-sl-git = nixpkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-sl";
    rev = "9c0fc48d49e0104392b31e02ee2e01506ebd39e9";
    sha256 = "1x8c6f9jr4shkx788xdhfbl2b5rnfh9lb50kr65zxa86r149r46y";
  };
  cardanopkgsOverrides = import (cardano-sl-git + /cardanopkgs.nix);

  /* The byron-adapter package needs cardano-sl (the library).
     cardano-sl packages need some special non-hackage dependencies, and
     some custom branches of various on-hackage dependencies.
     ouroboros-network is also a dependency of byron-adapter, and it uses
     QuickCheck 2.12.
     Many of the dependencies of cardano-sl are not compatible with this.
     We also use stm 2.5, which causes similar troubles.
     To make it all work, we give special overrides, and use the cardanopkgs
     overlay to get cardano-sl.
     TODO make this optional, until we can reliably get the cardano-sl overlay
     (no hard-coded path).
  */
  ourOverrides = import ./nix/haskell-overrides.nix { dontCheck = lib.dontCheck; };
  /* cannot override{ ... }.override{ ... } apparently... so we manually
     compose the overlays
  */
  overlay = self: super: (ourOverrides self super) // (cardanopkgsOverrides self super);
  /* Override callPackage so it doesn't check or do haddock, by default.
     Surely this is not the right way to do this.
  */
  ghc     = nixpkgs.haskell.packages.${compiler}.override ({
    overrides = self: super:
      let
        cp = path: args: lib.dontCheck (lib.dontHaddock (super.callPackage path args));
      in
        overlay (self // { callPackage = cp; }) super;
  });
  callPackage   = ghc.callPackage;
  callCabal2nix = ghc.callCabal2nix;

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

in
  rec {

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
      cleanSource (callCabal2nix "ouroboros-network" ./ouroboros-network { inherit io-sim io-sim-classes typed-transitions iohk-monitoring; })
    ))));

    ouroboros-consensus = docNoSeprateOutput(doHaddock(doTest(doBench(
      cleanSource (callCabal2nix "ouroboros-consensus" ./ouroboros-consensus { inherit io-sim-classes io-sim typed-transitions ouroboros-network; })
    ))));

  }
