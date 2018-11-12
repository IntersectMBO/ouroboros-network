{ mkDerivation, aeson, array, base, base16-bytestring, bytestring, cborg
, clock, containers, cryptonite, fingertree, free, hashable, memory, mtl
, network, process , QuickCheck, random, semigroups, stdenv, stm, serialise
, streaming, string-conv, tasty, tasty-quickcheck, text, transformers
, typed-transitions , unliftio, void, nixpkgs
}:
mkDerivation {
  pname = "ouroboros-network";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "ouroboros-network.cabal" "cabal.project" ];
  libraryHaskellDepends = [
    array aeson base base16-bytestring bytestring cborg clock containers
    cryptonite fingertree free hashable memory mtl network process QuickCheck
    random semigroups serialise stm streaming string-conv tasty
    tasty-quickcheck text transformers typed-transitions unliftio void
  ];
  testHaskellDepends = [
    array base bytestring cborg clock containers fingertree free hashable mtl
    process QuickCheck random semigroups stm tasty tasty-quickcheck text
    transformers typed-transitions void
  ];
  description = "A networking layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
