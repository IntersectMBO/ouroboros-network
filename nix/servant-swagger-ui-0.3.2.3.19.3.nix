{ mkDerivation, base, bytestring, file-embed-lzma, servant
, servant-server, servant-swagger-ui-core, stdenv, swagger2, text
}:
mkDerivation {
  pname = "servant-swagger-ui";
  version = "0.3.2.3.19.3";
  sha256 = "87ddb5982ce6b12698f9eff28b5d6fc2ebd00cb406bd48c8d0ff1951a1335e68";
  revision = "1";
  editedCabalFile = "0k2s6y93ii3d1myacq70ifpjf9q0mglxdr97wmxll6ixzsn7fjpl";
  libraryHaskellDepends = [
    base bytestring file-embed-lzma servant servant-server
    servant-swagger-ui-core swagger2 text
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger-ui";
  description = "Servant swagger ui";
  license = stdenv.lib.licenses.bsd3;
}
