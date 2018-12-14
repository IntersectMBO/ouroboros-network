{ mkDerivation, async, base, bytestring, free, QuickCheck, stdenv
, tasty, tasty-quickcheck, text, transformers, nixpkgs
}:
mkDerivation {
  pname = "typed-transitions";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "typed-transitions.cabal" ];
  libraryHaskellDepends = [ async base free text transformers ];
  testHaskellDepends = [
    async base bytestring QuickCheck tasty tasty-quickcheck text
  ];
  license = stdenv.lib.licenses.bsd3;
  enableSeparateDocOutput = false;
}
