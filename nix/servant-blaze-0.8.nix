{ mkDerivation, base, blaze-html, http-media, servant
, servant-server, stdenv, wai, warp
}:
mkDerivation {
  pname = "servant-blaze";
  version = "0.8";
  sha256 = "46ea88550123d765b2d09073370d0530a51878e7fdf2cf20b070be1f2f10ae94";
  revision = "2";
  editedCabalFile = "1cfla60vn4kk5gb7fawlp34jr2k6b2fprysq05561wdfv990x4bj";
  libraryHaskellDepends = [ base blaze-html http-media servant ];
  testHaskellDepends = [ base blaze-html servant-server wai warp ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Blaze-html support for servant";
  license = stdenv.lib.licenses.bsd3;
}
