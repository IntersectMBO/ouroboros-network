{ mkDerivation, attoparsec, base, bytestring, bytestring-builder
, deepseq, directory, filepath, HUnit, mtl, network, primitive
, process, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, time, transformers, vector
, zlib, zlib-bindings
}:
mkDerivation {
  pname = "io-streams";
  version = "1.5.0.1";
  sha256 = "5dcb3717933197a84f31be74abf545126b3d25eb0e0d64f722c480d3c46b2c8b";
  revision = "2";
  editedCabalFile = "1mcab95d6hm098myh9gp7sh10srigjphgvm8s9pfs7jg5hzghy14";
  configureFlags = [ "-fNoInteractiveTests" ];
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-builder network primitive
    process text time transformers vector zlib-bindings
  ];
  testHaskellDepends = [
    attoparsec base bytestring bytestring-builder deepseq directory
    filepath HUnit mtl network primitive process QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2 text
    time transformers vector zlib zlib-bindings
  ];
  description = "Simple, composable, and easy-to-use stream I/O";
  license = stdenv.lib.licenses.bsd3;
}
