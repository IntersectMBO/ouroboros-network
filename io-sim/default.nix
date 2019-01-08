{ mkDerivation, nixpkgs, array, base, containers, free, io-sim-classes
, psqueues, QuickCheck, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "io-sim";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ "LICENSE" ".hs" "io-sim.cabal" ];
  libraryHaskellDepends = [
    base containers free io-sim-classes psqueues
  ];
  testHaskellDepends = [
    array base containers io-sim-classes QuickCheck tasty
    tasty-quickcheck
  ];
  description = "A pure simlator for monadic concurrency with STM";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
