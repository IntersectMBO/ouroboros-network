{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network-transport, network-transport-tests, stdenv, stm
}:
mkDerivation {
  pname = "network-transport-inmemory";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport-inmemory";
    sha256 = "0d19dnqxcnmd11v9drrvsxqfy2w8vrsccw2asd2jvxqs4qqfg4b9";
    rev = "ae3ba7e656c199658e85170dc9d4487e1f31729a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers data-accessor network-transport stm
  ];
  testHaskellDepends = [
    base network-transport network-transport-tests
  ];
  homepage = "http://haskell-distributed.github.com";
  description = "In-memory instantiation of Network.Transport";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
