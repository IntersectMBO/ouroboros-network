{
  mkDerivation
, base
, basement
, bytestring
, cryptonite
, cryptonite-openssl
, deepseq
, fetchgit
, foundation
, hashable
, integer-gmp
, memory
, stdenv
}:
mkDerivation {

pname = "cardano-crypto";
version = "1.1.0";
src = fetchgit {

url = "https://github.com/angerman/cardano-crypto";
sha256 = "02kisrjavvi4hr2kyq7znd7y3y5c4pbl6w8gr5a58fl03m69sngr";
rev = "1e436fe8b69e7c8b399937db394d99229fcd775c";
fetchSubmodules = true;

};
isLibrary = true;
isExecutable = true;
libraryHaskellDepends = [
base
basement
bytestring
cryptonite
cryptonite-openssl
deepseq
foundation
hashable
integer-gmp
memory
];
doHaddock = false;
doCheck = false;
homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
description = "Cryptography primitives for cardano";
license = stdenv.lib.licenses.mit;

}
