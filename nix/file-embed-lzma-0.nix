{ mkDerivation, base, base-compat, bytestring, directory, filepath
, lzma, stdenv, template-haskell, text, th-lift-instances
, transformers
}:
mkDerivation {
  pname = "file-embed-lzma";
  version = "0";
  sha256 = "e86cf44f747cf403898158e9fdf9342871e293097a29679fcf587aed497f0c77";
  revision = "2";
  editedCabalFile = "0dmg69gsj2k9lf112bvqw6z2w8hl0p1lx5zxdvlvk85bb3qz6304";
  libraryHaskellDepends = [
    base base-compat bytestring directory filepath lzma
    template-haskell text th-lift-instances transformers
  ];
  testHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/phadej/file-embed-lzma";
  description = "Use Template Haskell to embed (LZMA compressed) data";
  license = stdenv.lib.licenses.bsd3;
}
