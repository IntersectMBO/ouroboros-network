{ mkDerivation, base, hedgehog, stdenv, tagged, tasty
, tasty-expected-failure
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "0.2.0.0";
  sha256 = "5a107fc3094efc50663e4634331a296281318b38c9902969c2d2d215d754a182";
  revision = "6";
  editedCabalFile = "0d7s1474pvnyad6ilr5rvpama7s468ya9ns4ksbl0827z9vvga43";
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  homepage = "https://github.com/qfpl/tasty-hedgehog";
  description = "Integration for tasty and hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
