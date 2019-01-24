{ mkDerivation, base, containers, ghc-prim, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ekg-core";
  version = "0.1.1.6";
  sha256 = "66a8dd79ad27659052168f08dd41fabb8593e364de00fb857ef5cc943acd5742";
  libraryHaskellDepends = [
    base containers ghc-prim text unordered-containers
  ];
  benchmarkHaskellDepends = [ base ];
  homepage = "https://github.com/tibbe/ekg-core";
  description = "Tracking of system metrics";
  license = stdenv.lib.licenses.bsd3;
}
