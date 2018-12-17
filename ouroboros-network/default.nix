{ mkDerivation, array, async, base, bytestring, cborg, clock
, containers, fingertree, free, hashable, mtl, network, pipes, process
, psqueues, QuickCheck, serialise, stdenv, stm, tasty
, tasty-quickcheck, text, typed-transitions, iohk-monitoring, nixpkgs
}:
mkDerivation {
  pname = "ouroboros-network";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ouroboros-network.cabal" ];
  libraryHaskellDepends = [
    array async base bytestring cborg clock containers fingertree free hashable
    mtl network pipes process psqueues QuickCheck serialise stm text
    typed-transitions iohk-monitoring
  ];
  testHaskellDepends = [
    array base containers fingertree mtl pipes QuickCheck tasty tasty-quickcheck
    typed-transitions
  ];
  description = "A networking layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
