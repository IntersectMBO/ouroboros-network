{ mkDerivation, base, containers, contravariant, stdenv }:
mkDerivation {
  pname = "lrucache";
  version = "1.2.0.1";
  sha256 = "fc1ab2375eeaae181d838095354d3ef77d4072815006a285dd39a165a5855b85";
  libraryHaskellDepends = [ base containers contravariant ];
  homepage = "http://github.com/chowells79/lrucache";
  description = "a simple, pure LRU cache";
  license = stdenv.lib.licenses.bsd3;
}
