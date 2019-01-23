{ mkDerivation, aeson, base, bytestring, containers, parsec, pretty
, QuickCheck, stdenv, tasty, tasty-quickcheck, unordered-containers
, vector
}:
mkDerivation {
  pname = "canonical-json";
  version = "0.5.0.1";
  sha256 = "9243e144754c2918d410c1fc496c54520671b9a1594060eb34d46aa79271a2e4";
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
