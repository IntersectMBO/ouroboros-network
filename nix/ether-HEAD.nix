{ mkDerivation, base, criterion, deepseq, exceptions, fetchgit
, ghc-prim, lens, mmorph, monad-control, mtl, QuickCheck
, reflection, stdenv, tagged, tasty, tasty-quickcheck
, template-haskell, transformers, transformers-base
, transformers-lift, writer-cps-mtl
}:
mkDerivation {
  pname = "ether";
  version = "0.5.1.0";
  src = fetchgit {
    url = "https://github.com/int-index/ether";
    sha256 = "0h2md24q9dhxh5r79dy7shry7yxgwf45735fqwlx3j2z0znq9vxs";
    rev = "84c1d560da241c8111d1a3c98d9a896f0c62087b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base exceptions mmorph monad-control mtl reflection tagged
    template-haskell transformers transformers-base transformers-lift
    writer-cps-mtl
  ];
  testHaskellDepends = [
    base ghc-prim lens mtl QuickCheck tasty tasty-quickcheck
    transformers
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq lens mtl transformers
  ];
  homepage = "https://int-index.github.io/ether/";
  description = "Monad transformers and classes";
  license = stdenv.lib.licenses.bsd3;
}
