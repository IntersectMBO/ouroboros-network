{ mkDerivation, base, HUnit, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "fingertree";
  version = "0.1.4.2";
  sha256 = "95a948341570bad5a9b2468c388c0eb2c20c57e10dd8fbfc994c7b8764b36a7f";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  description = "Generic finger-tree structure, with example instances";
  license = stdenv.lib.licenses.bsd3;
}
