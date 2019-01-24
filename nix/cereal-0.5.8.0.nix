{ mkDerivation, array, base, bytestring, containers, ghc-prim
, QuickCheck, stdenv, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "cereal";
  version = "0.5.8.0";
  sha256 = "37d3668aba37babc83321aa1538841da041476b5423722e6f5210c4e5e014282";
  libraryHaskellDepends = [
    array base bytestring containers ghc-prim
  ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/GaloisInc/cereal";
  description = "A binary serialization library";
  license = stdenv.lib.licenses.bsd3;
}
