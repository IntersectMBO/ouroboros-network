{ mkDerivation, base, hspec-core, hspec-discover
, hspec-expectations, QuickCheck, stdenv
}:
mkDerivation {
  pname = "hspec";
  version = "2.6.0";
  sha256 = "d1ac36509ab32f55eb7be8bee7a3c880fbf1d0654ff67cc416050be716509463";
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}
