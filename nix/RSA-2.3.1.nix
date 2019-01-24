{ mkDerivation, base, binary, bytestring, crypto-api
, crypto-pubkey-types, DRBG, QuickCheck, SHA, stdenv, tagged
, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "RSA";
  version = "2.3.1";
  sha256 = "5c929c14de467a9f032641e1b79cbb31a796615c89bf90d059aee5b04eb3671a";
  libraryHaskellDepends = [
    base binary bytestring crypto-api crypto-pubkey-types SHA
  ];
  testHaskellDepends = [
    base binary bytestring crypto-api crypto-pubkey-types DRBG
    QuickCheck SHA tagged test-framework test-framework-quickcheck2
  ];
  description = "Implementation of RSA, using the padding schemes of PKCS#1 v2.1.";
  license = stdenv.lib.licenses.bsd3;
}
