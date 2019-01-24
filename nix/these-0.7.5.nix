{ mkDerivation, aeson, base, bifunctors, binary, containers
, data-default-class, deepseq, hashable, keys, mtl, profunctors
, QuickCheck, quickcheck-instances, semigroupoids, stdenv, tasty
, tasty-quickcheck, transformers, transformers-compat
, unordered-containers, vector, vector-instances
}:
mkDerivation {
  pname = "these";
  version = "0.7.5";
  sha256 = "dbac2412ad609d2ccd180722ac73a3f0fb2df300460a78d687660135efec35fb";
  libraryHaskellDepends = [
    aeson base bifunctors binary containers data-default-class deepseq
    hashable keys mtl profunctors QuickCheck semigroupoids transformers
    transformers-compat unordered-containers vector vector-instances
  ];
  testHaskellDepends = [
    aeson base bifunctors binary containers hashable QuickCheck
    quickcheck-instances tasty tasty-quickcheck transformers
    unordered-containers vector
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "An either-or-both data type & a generalized 'zip with padding' typeclass";
  license = stdenv.lib.licenses.bsd3;
}
