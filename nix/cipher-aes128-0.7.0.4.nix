{ mkDerivation, base, bytestring, Cabal, cereal, criterion
, crypto-api, entropy, process, stdenv, tagged
}:
mkDerivation {
  pname = "cipher-aes128";
  version = "0.7.0.4";
  sha256 = "cd8d8987c1a1839f3c66e655277981083be85489d34b6b47f95d7e82d2d10285";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal process ];
  libraryHaskellDepends = [
    base bytestring cereal crypto-api tagged
  ];
  benchmarkHaskellDepends = [
    base bytestring cereal criterion crypto-api entropy tagged
  ];
  homepage = "https://github.com/TomMD/cipher-aes128";
  description = "AES and common modes using AES-NI when available";
  license = stdenv.lib.licenses.bsd3;
}
