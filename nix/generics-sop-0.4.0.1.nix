{ mkDerivation, base, criterion, deepseq, ghc-prim, sop-core
, stdenv, template-haskell
}:
mkDerivation {
  pname = "generics-sop";
  version = "0.4.0.1";
  sha256 = "dc99fa6c597b7ce256bdbdfc89fc615f26013e25256dd7e813f05b7845b61398";
  libraryHaskellDepends = [
    base ghc-prim sop-core template-haskell
  ];
  testHaskellDepends = [ base ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  description = "Generic Programming using True Sums of Products";
  license = stdenv.lib.licenses.bsd3;
}
