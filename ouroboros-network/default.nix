{ mkDerivation, aeson, array, async, base, base16-bytestring
, bytestring, cborg, clock, containers, cryptonite, directory
, exceptions, filepath, fingertree, free, hashable, memory, mtl
, network, optparse-applicative, pipes, process, psqueues
, QuickCheck, random, semigroups, serialise, stdenv, stm
, string-conv, tasty, tasty-expected-failure, tasty-hunit
, tasty-quickcheck, temporary, text, time, transformers
, typed-transitions, unix, unliftio, vector, void
}:
mkDerivation {
  pname = "ouroboros-network";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base base16-bytestring bytestring cborg clock
    containers cryptonite directory exceptions filepath fingertree free
    hashable memory mtl network pipes process psqueues QuickCheck
    random semigroups serialise stm text time transformers
    typed-transitions unix unliftio vector void
  ];
  executableHaskellDepends = [
    aeson async base bytestring cborg containers cryptonite directory
    mtl optparse-applicative process QuickCheck serialise stm
    string-conv text typed-transitions unix
  ];
  testHaskellDepends = [
    array base bytestring cborg clock containers cryptonite directory
    exceptions filepath fingertree free hashable mtl pipes process
    QuickCheck random semigroups serialise stm tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck temporary text
    transformers typed-transitions void
  ];
  description = "A networking layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
}
