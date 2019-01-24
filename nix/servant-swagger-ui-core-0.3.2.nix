{ mkDerivation, base, blaze-markup, bytestring, http-media, servant
, servant-blaze, servant-server, stdenv, swagger2, text
, transformers, transformers-compat, wai-app-static
}:
mkDerivation {
  pname = "servant-swagger-ui-core";
  version = "0.3.2";
  sha256 = "a2cd0e8e68c5de21aea54735f891c4c6e54007c85e93dffd42b89aba419a3ca8";
  revision = "1";
  editedCabalFile = "0dd97qvi5w1y90ln58pk0y2vb5f1bhwsix9ym3cnnq8h0snfda4p";
  libraryHaskellDepends = [
    base blaze-markup bytestring http-media servant servant-blaze
    servant-server swagger2 text transformers transformers-compat
    wai-app-static
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger-ui";
  description = "Servant swagger ui core components";
  license = stdenv.lib.licenses.bsd3;
}
