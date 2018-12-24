{ mkDerivation, base, HUnit, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "fingertree";
  version = "0.1.4.2";
  sha256 = "0zvandj8fysck7ygpn0dw5bhrhmj1s63i326nalxbfkh2ls4iacm";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  description = "Generic finger-tree structure, with example instances";
  license = stdenv.lib.licenses.bsd3;
}