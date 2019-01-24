{
  mkDerivation
, base
, bytestring
, directory
, fetchgit
, rocksdb
, stdenv
}:
mkDerivation {

pname = "rocksdb-haskell-ng";
version = "0.0.0";
src = fetchgit {

url = "https://github.com/input-output-hk/rocksdb-haskell-ng.git";
sha256 = "02jvri8ik8jgrxwa6qmh3xcwqvm4s27iv3sxpjpny79nlhlxvfzp";
rev = "49f501a082d745f3b880677220a29cafaa181452";
fetchSubmodules = true;

};
libraryHaskellDepends = [
base
bytestring
directory
];
librarySystemDepends = [
rocksdb
];
doHaddock = false;
doCheck = false;
license = stdenv.lib.licenses.bsd3;

}
