{ mkDerivation, base, binary, bytestring, deepseq, fetchgit
, hashable, stdenv, transformers
}:
mkDerivation {
  pname = "network-transport";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport";
    sha256 = "00p4v8l69mh0219l2qnj5zna10q6ngvhrb7v114rmxml1zsp9nsp";
    rev = "0b8f5a7bec389a4ffa653792cfd203c742a0857b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary bytestring deepseq hashable transformers
  ];
  homepage = "http://haskell-distributed.github.com";
  description = "Network abstraction layer";
  license = stdenv.lib.licenses.bsd3;
}
