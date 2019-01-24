{ mkDerivation, base, bytestring, file-embed-lzma, servant
, servant-server, servant-swagger-ui-core, stdenv, swagger2, text
}:
mkDerivation {
  pname = "servant-swagger-ui-redoc";
  version = "0.3.2.1.22.2";
  sha256 = "e09919e7518f8f5b00868eac0c4f80212b5a4950d2c10112696f52446e369934";
  revision = "1";
  editedCabalFile = "030zf1z5h96d40ifwagxblz1dij2ypbcqyy0wpqvjqbianyqgcim";
  libraryHaskellDepends = [
    base bytestring file-embed-lzma servant servant-server
    servant-swagger-ui-core swagger2 text
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger-ui";
  description = "Servant swagger ui: ReDoc theme";
  license = stdenv.lib.licenses.bsd3;
}
