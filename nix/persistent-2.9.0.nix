{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-html, blaze-markup, bytestring, conduit, containers
, fast-logger, hspec, http-api-data, monad-control, monad-logger
, mtl, old-locale, path-pieces, resource-pool, resourcet
, scientific, silently, stdenv, tagged, template-haskell, text
, time, transformers, unliftio-core, unordered-containers, vector
, void
}:
mkDerivation {
  pname = "persistent";
  version = "2.9.0";
  sha256 = "e7865ceb4aa1e93ca8c65c789f92c8046a39ecf41283682bcace33e89b77f261";
  revision = "2";
  editedCabalFile = "1szx008irw7w2h9qz443mml06sg6w9vazbxxyi67d91hyjlgca2j";
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-html blaze-markup
    bytestring conduit containers fast-logger http-api-data
    monad-logger mtl old-locale path-pieces resource-pool resourcet
    scientific silently tagged template-haskell text time transformers
    unliftio-core unordered-containers vector void
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-html bytestring
    conduit containers fast-logger hspec http-api-data monad-control
    monad-logger mtl old-locale path-pieces resource-pool resourcet
    scientific tagged template-haskell text time transformers
    unordered-containers vector
  ];
  homepage = "http://www.yesodweb.com/book/persistent";
  description = "Type-safe, multi-backend data serialization";
  license = stdenv.lib.licenses.mit;
}
