{ mkDerivation, base, hspec, hspec-core, QuickCheck, stdenv, tasty
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "tasty-hspec";
  version = "1.1.5.1";
  sha256 = "fe889ec0f7b3991c46a07d9ff9cf09608a73a18f434a7480d2a09c79e56f3345";
  revision = "1";
  editedCabalFile = "18k4p273qnvfmk5cbm89rjqr0v03v0q22q7bbl7z3bxpwnnkmhqf";
  libraryHaskellDepends = [
    base hspec hspec-core QuickCheck tasty tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/mitchellwrosen/tasty-hspec";
  description = "Hspec support for the Tasty test framework";
  license = stdenv.lib.licenses.bsd3;
}
