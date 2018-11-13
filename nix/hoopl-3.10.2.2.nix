{ mkDerivation, base, containers, filepath, mtl, parsec, stdenv
, test-framework, test-framework-hunit
}:
mkDerivation {
  pname = "hoopl";
  version = "3.10.2.2";
  sha256 = "097b1316d5f1c8ffe71133223209eb2b095fe13f43dc01d1fe43fd8a545a2b97";
  revision = "2";
  editedCabalFile = "0j6pz4jzhvviyrhhn1j22ikmjvzrg60nzvq26lbpkcb6y4q6rlyx";
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [
    base containers filepath mtl parsec test-framework
    test-framework-hunit
  ];
  homepage = "https://github.com/haskell/hoopl";
  description = "A library to support dataflow analysis and optimization";
  license = stdenv.lib.licenses.bsd3;
}
