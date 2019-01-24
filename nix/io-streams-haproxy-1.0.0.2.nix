{ mkDerivation, attoparsec, base, bytestring, HUnit, io-streams
, network, stdenv, test-framework, test-framework-hunit
, transformers
}:
mkDerivation {
  pname = "io-streams-haproxy";
  version = "1.0.0.2";
  sha256 = "77814f8258b5c32707a13e0d30ab2e144e7ad073aee821d6def65554024ed086";
  revision = "4";
  editedCabalFile = "06c51a057n5bc9xfbp2m4jz5ds4z1xvmsx5mppch6qfwbz7x5i9l";
  libraryHaskellDepends = [
    attoparsec base bytestring io-streams network transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit io-streams network test-framework
    test-framework-hunit transformers
  ];
  homepage = "http://snapframework.com/";
  description = "HAProxy protocol 1.5 support for io-streams";
  license = stdenv.lib.licenses.bsd3;
}
