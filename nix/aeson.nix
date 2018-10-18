{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, contravariant, deepseq
, directory, dlist, fetchgit, filepath, generic-deriving, ghc-prim
, hashable, hashable-time, integer-logarithms, primitive
, QuickCheck, quickcheck-instances, scientific, stdenv, tagged
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, time, time-locale-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.1.0";
  src = fetchgit {
    url = "https://github.com/coot/aeson";
    sha256 = "10xx3q4g3bgmpl38ynac06bbrx4hv39q36yb13ccq7zr45j9m2xw";
    rev = "980c19f99eaf8a44bf719c67687a74f5552854a6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers contravariant
    deepseq dlist ghc-prim hashable primitive scientific tagged
    template-haskell text th-abstraction time time-locale-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers directory dlist filepath generic-deriving
    ghc-prim hashable hashable-time integer-logarithms QuickCheck
    quickcheck-instances scientific tagged tasty tasty-hunit
    tasty-quickcheck template-haskell text time time-locale-compat
    unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
