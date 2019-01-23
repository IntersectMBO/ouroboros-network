{ mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.6.0";
  sha256 = "82d9417cfe6df8a0289f7bb24b17138f399571997cc9d0e1439cfa7b7e79059f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = stdenv.lib.licenses.mit;
}
