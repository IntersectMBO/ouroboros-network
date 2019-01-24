{ mkDerivation, base, binary, bytestring, containers, contravariant
, cryptonite, data-default, errors, extra, fetchgit, HUnit, memory
, MonadRandom, mtl, network, QuickCheck, quickcheck-instances
, random, random-shuffle, stdenv, stm, tasty, tasty-hunit
, tasty-quickcheck, time, transformers, transformers-compat
}:
mkDerivation {
  pname = "kademlia";
  version = "1.1.0.1";
  src = fetchgit {
    url = "https://github.com/avieth/kademlia";
    sha256 = "067yqv7dr5s649s3xihgzcq5df7j5kpabnd5mq0zll5ar45v6qdr";
    rev = "38a0575bb303804461f4b6176ca38eba81adbd79";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers contravariant cryptonite extra memory
    MonadRandom mtl network random random-shuffle stm time transformers
  ];
  executableHaskellDepends = [
    base binary bytestring containers data-default extra MonadRandom
    mtl network random random-shuffle transformers transformers-compat
  ];
  testHaskellDepends = [
    base binary bytestring containers data-default errors extra HUnit
    MonadRandom mtl network QuickCheck quickcheck-instances random
    random-shuffle stm tasty tasty-hunit tasty-quickcheck time
    transformers transformers-compat
  ];
  homepage = "https://github.com/serokell/kademlia";
  description = "An implementation of the Kademlia DHT Protocol";
  license = stdenv.lib.licenses.bsd3;
}
