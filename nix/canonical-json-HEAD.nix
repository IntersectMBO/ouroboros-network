{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, parsec, pretty, QuickCheck, stdenv, tasty, tasty-quickcheck
, unordered-containers, vector
}:
mkDerivation {
  pname = "canonical-json";
  version = "0.5.0.2";
  src = fetchgit {
    url = "https://github.com/well-typed/canonical-json";
    sha256 = "0alwbi9xqaj6fmwzs6lr2drqrnhlnp13d9k2qkl5ga7h4grz9zcr";
    rev = "a4016204609ae6708f007012c68bf8c8fb3f6e6c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers parsec pretty
  ];
  testHaskellDepends = [
    aeson base bytestring QuickCheck tasty tasty-quickcheck
    unordered-containers vector
  ];
  homepage = "https://github.com/well-typed/canonical-json";
  description = "Canonical JSON for signing and hashing JSON values";
  license = stdenv.lib.licenses.bsd3;
}
