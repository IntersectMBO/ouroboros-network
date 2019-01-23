{ mkDerivation, aeson, aeson-pretty, base, base-compat, bytestring
, Cabal, cabal-doctest, directory, doctest, filepath, hspec
, hspec-discover, http-media, insert-ordered-containers, lens
, QuickCheck, servant, singleton-bool, stdenv, swagger2
, template-haskell, text, time, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "servant-swagger";
  version = "1.1.7";
  sha256 = "e31a1020553c2879047e7d15cd1b57b4ec216606554fdecd62e0f4521e81de36";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson aeson-pretty base base-compat bytestring hspec http-media
    insert-ordered-containers lens QuickCheck servant singleton-bool
    swagger2 text unordered-containers
  ];
  testHaskellDepends = [
    aeson base base-compat directory doctest filepath hspec lens
    QuickCheck servant swagger2 template-haskell text time utf8-string
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-servant/servant-swagger";
  description = "Generate Swagger specification for your servant API";
  license = stdenv.lib.licenses.bsd3;
}
