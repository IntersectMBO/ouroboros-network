{ mkDerivation, array, base, bytestring, cborg, clock, containers
, fingertree, free, hashable, mtl, process, QuickCheck, random, semigroups
, stdenv, stm, tasty, tasty-quickcheck, text, transformers, typed-transitions
, void, nixpkgs
}:
mkDerivation {
  pname = "ouroboros-network";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "ouroboros-network.cabal" "cabal.project" ];
  libraryHaskellDepends = [
    array base bytestring cborg clock containers fingertree free
    hashable mtl process QuickCheck random semigroups stm tasty tasty-quickcheck
    text transformers typed-transitions void
  ];
  testHaskellDepends = [
    array base bytestring cborg clock containers fingertree free hashable
    mtl process QuickCheck random semigroups stm tasty tasty-quickcheck text
    transformers void
  ];
  description = "A networking layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
