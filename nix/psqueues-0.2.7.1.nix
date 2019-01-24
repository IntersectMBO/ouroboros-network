{ mkDerivation, array, base, containers, criterion, deepseq
, fingertree-psqueue, ghc-prim, hashable, HUnit, mtl, PSQueue
, QuickCheck, random, stdenv, tagged, test-framework
, test-framework-hunit, test-framework-quickcheck2
, unordered-containers
}:
mkDerivation {
  pname = "psqueues";
  version = "0.2.7.1";
  sha256 = "047e42ecd50d09fef99d1db9f8b1e511b64ea4b41afc435ad5fdd373d2ea8ec1";
  revision = "1";
  editedCabalFile = "0336d9ckixv4n23vy5l3xk0wavfn3z9xk105gig0zv70b3jh3r3y";
  libraryHaskellDepends = [ base deepseq ghc-prim hashable ];
  testHaskellDepends = [
    array base deepseq ghc-prim hashable HUnit QuickCheck tagged
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq fingertree-psqueue ghc-prim
    hashable mtl PSQueue random unordered-containers
  ];
  description = "Pure priority search queues";
  license = stdenv.lib.licenses.bsd3;
}
