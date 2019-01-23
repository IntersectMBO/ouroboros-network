{
  mkDerivation
, base
, bifunctors
, binary
, bytestring
, cardano-crypto
, cryptonite
, ed25519
, either
, fetchgit
, filepath
, lens
, memory
, mtl
, operational
, parsec
, stdenv
, transformers
}:
mkDerivation {

pname = "plutus-prototype";
version = "0.1.0.0";
src = fetchgit {

url = "https://github.com/avieth/plutus-prototype";
sha256 = "1s932rghn4zn441waansp408b5bwk20rc1wsa5693a2nwnp4dijw";
rev = "d094be301195fcd8ab864d793f114970426a4478";
fetchSubmodules = true;

};
enableSeparateDataOutput = true;
libraryHaskellDepends = [
base
bifunctors
binary
bytestring
cardano-crypto
cryptonite
ed25519
either
filepath
lens
memory
mtl
operational
parsec
transformers
];
doHaddock = false;
doCheck = false;
homepage = "iohk.io";
description = "Prototype of the Plutus language";
license = stdenv.lib.licenses.mit;
}
