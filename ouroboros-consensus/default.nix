{ mkDerivation, aeson, async, base, base16-bytestring, bytestring
, cborg, containers, cryptonite, directory, exceptions, fgl
, filepath, graphviz ,memory, mtl, optparse-applicative
, ouroboros-network, QuickCheck, serialise, stdenv, stm
, string-conv, tasty, tasty-hunit, tasty-quickcheck, temporary
, text, time, typed-transitions, unix , vector, nixpkgs
}:
mkDerivation {
  pname = "ouroboros-consensus";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ouroboros-consensus.cabal" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring cborg containers cryptonite
    directory exceptions filepath memory mtl
    ouroboros-network QuickCheck serialise text time
    typed-transitions unix vector
  ];
  executableHaskellDepends = [
    aeson async base bytestring cborg containers cryptonite directory
    mtl optparse-applicative ouroboros-network serialise stm
    string-conv text typed-transitions unix
  ];
  testHaskellDepends = [
    base bytestring containers cryptonite directory exceptions fgl
    graphviz mtl ouroboros-network QuickCheck serialise tasty
    tasty-hunit tasty-quickcheck temporary typed-transitions
  ];
  description = "Consensus layer for the Ouroboros blockchain protocol";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
