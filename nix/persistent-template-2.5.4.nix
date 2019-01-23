{ mkDerivation, aeson, aeson-compat, base, bytestring, containers
, ghc-prim, hspec, http-api-data, monad-control, monad-logger
, path-pieces, persistent, QuickCheck, stdenv, tagged
, template-haskell, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "persistent-template";
  version = "2.5.4";
  sha256 = "4cae740ce92f98cb3ae9e092e740753394d5687b887399ee5f87af7f3c730a01";
  revision = "3";
  editedCabalFile = "12f4pqxwfv2li78sd9s56p66xd0w465cmjycpkqvg8n1rjxkc8vs";
  libraryHaskellDepends = [
    aeson aeson-compat base bytestring containers ghc-prim
    http-api-data monad-control monad-logger path-pieces persistent
    tagged template-haskell text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring hspec persistent QuickCheck text transformers
  ];
  homepage = "http://www.yesodweb.com/book/persistent";
  description = "Type-safe, non-relational, multi-backend persistence";
  license = stdenv.lib.licenses.mit;
}
