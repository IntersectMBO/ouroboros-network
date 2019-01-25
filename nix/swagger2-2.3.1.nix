{ mkDerivation, aeson, base, base-compat-batteries, bytestring
, Cabal, cabal-doctest, containers, cookie, doctest, generics-sop
, Glob, hashable, hspec, hspec-discover, http-media, HUnit
, insert-ordered-containers, lens, mtl, network, QuickCheck
, quickcheck-instances, scientific, stdenv, template-haskell, text
, time, transformers, transformers-compat, unordered-containers
, utf8-string, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.3.1";
  sha256 = "c61fa150dfd4e6f8c17ef66044b7fd1c15f404fc7a91e4dae25e9fb41789271c";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base base-compat-batteries bytestring containers cookie
    generics-sop hashable http-media insert-ordered-containers lens mtl
    network QuickCheck scientific template-haskell text time
    transformers transformers-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    aeson base base-compat-batteries bytestring containers doctest Glob
    hashable hspec HUnit insert-ordered-containers lens mtl QuickCheck
    quickcheck-instances template-haskell text time
    unordered-containers utf8-string vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/GetShopTV/swagger2";
  description = "Swagger 2.0 data model";
  license = stdenv.lib.licenses.bsd3;
}
