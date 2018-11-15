{ mkDerivation, aeson, async, array, base, base16-bytestring, binary
, bytestring , cborg , clock, containers, cryptonite, fingertree, free
, hashable, memory, mtl, network, process, psqueues, QuickCheck, random
, semigroups , stdenv, stm, serialise , string-conv, tasty
, tasty-quickcheck , text , transformers, unliftio, void
, nixpkgs
}:
mkDerivation {
  pname = "ouroboros-network";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "ouroboros-network.cabal" "cabal.project" ];
  libraryHaskellDepends = [
    array aeson async base base16-bytestring binary bytestring cborg clock
    containers cryptonite fingertree free hashable memory mtl network process
    psqueues QuickCheck random semigroups serialise stm string-conv tasty
    tasty-quickcheck text transformers unliftio void
  ];
  testHaskellDepends = [
    array async base binary bytestring cborg clock containers
    fingertree free hashable mtl network process QuickCheck random semigroups
    stm tasty tasty-quickcheck text transformers void
  ];
  description = "A networking layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
