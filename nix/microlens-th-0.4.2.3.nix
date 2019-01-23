{ mkDerivation, base, containers, microlens, stdenv
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "microlens-th";
  version = "0.4.2.3";
  sha256 = "321018c6c0aad3f68eb26f6c7e7a518db43039e3f8f19c4634ceb4c7f8051c8f";
  libraryHaskellDepends = [
    base containers microlens template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base microlens ];
  homepage = "http://github.com/aelve/microlens";
  description = "Automatic generation of record lenses for microlens";
  license = stdenv.lib.licenses.bsd3;
}
