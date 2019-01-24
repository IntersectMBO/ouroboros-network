{ mkDerivation, base, bytestring, directory, doctest, hspec, HUnit
, stdenv, unix
}:
mkDerivation {
  pname = "network";
  version = "2.8.0.0";
  sha256 = "c8905268b7e3b4cf624a40245bf11b35274a6dd836a5d4d531b5760075645303";
  libraryHaskellDepends = [ base bytestring unix ];
  testHaskellDepends = [
    base bytestring directory doctest hspec HUnit
  ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = stdenv.lib.licenses.bsd3;
  /* preConfigure = ''
    ls -R
    autoreconf
    '';
  buildTools = [ autoconf automake ]; */
}
