{ mkDerivation, aeson, base, base-compat, hashable, lens
, QuickCheck, semigroupoids, semigroups, stdenv, tasty
, tasty-quickcheck, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "insert-ordered-containers";
  version = "0.2.1.0";
  sha256 = "d71d126bf455898492e1d2ba18b2ad04453f8b0e4daff3926a67f0560a712298";
  revision = "9";
  editedCabalFile = "02d4zqyb9dbahkpcbpgxylrc5xxc0zbw1awj5w0jyrql2g2b6a5f";
  libraryHaskellDepends = [
    aeson base base-compat hashable lens semigroupoids semigroups text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base base-compat hashable lens QuickCheck semigroupoids
    semigroups tasty tasty-quickcheck text transformers
    unordered-containers
  ];
  homepage = "https://github.com/phadej/insert-ordered-containers#readme";
  description = "Associative containers retaining insertion order for traversals";
  license = stdenv.lib.licenses.bsd3;
}
