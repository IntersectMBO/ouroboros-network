{ mkDerivation, base, basement, bytestring, cryptonite
, cryptonite-openssl, deepseq, fetchgit, foundation, gauge
, hashable, integer-gmp, memory, stdenv
}:
mkDerivation {
  pname = "cardano-crypto";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/avieth/cardano-crypto";
    sha256 = "11s95894hxsj3g6ka9mhrawhcihdayfapzw0c5bpm5fgs741b6pd";
    rev = "784dbd11f213af58d70f1306399d941eb222abf8";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base basement bytestring cryptonite cryptonite-openssl deepseq
    foundation hashable integer-gmp memory
  ];
  testHaskellDepends = [
    base basement bytestring cryptonite foundation memory
  ];
  benchmarkHaskellDepends = [
    base bytestring cryptonite gauge memory
  ];
  homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
  description = "Cryptography primitives for cardano";
  license = stdenv.lib.licenses.mit;
}
