{ mkDerivation, array, base, containers, mtl, QuickCheck, random
, stdenv, syb
}:
mkDerivation {
  pname = "ChasingBottoms";
  version = "1.3.1.5";
  sha256 = "60f43e0956459606e3432ab528bada79503f928c9fa26e52deaea8961613d341";
  libraryHaskellDepends = [
    base containers mtl QuickCheck random syb
  ];
  testHaskellDepends = [
    array base containers mtl QuickCheck random syb
  ];
  description = "For testing partial and infinite values";
  license = stdenv.lib.licenses.mit;
}
