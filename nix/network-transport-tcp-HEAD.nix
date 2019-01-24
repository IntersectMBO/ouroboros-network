{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network, network-transport, network-transport-tests
, stdenv, uuid
}:
mkDerivation {
  pname = "network-transport-tcp";
  version = "0.6.0";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport-tcp";
    sha256 = "0rygmgj3s865ry045cg8dnnf9idsj6rr1wzsxjc97lmfcv2a12z5";
    rev = "da5e1544ead1d1b70ca4b7a54fc5f02b1a9a98c9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers data-accessor network network-transport
    uuid
  ];
  testHaskellDepends = [
    base bytestring network network-transport network-transport-tests
  ];
  homepage = "http://haskell-distributed.github.com";
  description = "TCP instantiation of Network.Transport";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
