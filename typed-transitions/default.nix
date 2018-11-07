{ mkDerivation, stdenv, base, bytestring, async, free, network, QuickCheck
, tasty, tasty-quickcheck, text, transformers, nixpkgs
}:
mkDerivation {
  pname = "typed-transitions";
  version = "0.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "typed-transitions.cabal" "cabal.project" ];
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base transformers free text
  ];
  testHaskellDepends = [
    base async bytestring QuickCheck tasty tasty-quickcheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
