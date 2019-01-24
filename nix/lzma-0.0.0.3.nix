{ mkDerivation, base, bytestring, HUnit, lzma, QuickCheck, stdenv
, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "lzma";
  version = "0.0.0.3";
  sha256 = "af8321c3511bde3e2745093fa3bd74c642e386db7d2e7c43b3a54814f1338144";
  revision = "3";
  editedCabalFile = "1sify6gnsalyp6dakfzi0mdy5jcz2kcp9jsdsgkmxd40nfzgd44m";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ lzma ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/hvr/lzma";
  description = "LZMA/XZ compression and decompression";
  license = stdenv.lib.licenses.bsd3;
}
