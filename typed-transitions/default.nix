{ mkDerivation, async, base, bytestring, QuickCheck, stdenv, tasty
, tasty-quickcheck, nixpkgs
}:
mkDerivation {
  pname = "typed-transitions";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "typed-transitions.cabal" ];
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    async base bytestring QuickCheck tasty tasty-quickcheck
  ];
  license = stdenv.lib.licenses.bsd3;
  enableSeparateDocOutput = false;
}
