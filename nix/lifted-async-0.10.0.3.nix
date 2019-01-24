{ mkDerivation, async, base, constraints, criterion, deepseq, HUnit
, lifted-base, monad-control, mtl, stdenv, tasty
, tasty-expected-failure, tasty-hunit, tasty-th, transformers-base
}:
mkDerivation {
  pname = "lifted-async";
  version = "0.10.0.3";
  sha256 = "83d09c355cf7c5d35f179f6f084524f451966ed29beac721f0500ee607822b8c";
  libraryHaskellDepends = [
    async base constraints lifted-base monad-control transformers-base
  ];
  testHaskellDepends = [
    async base HUnit lifted-base monad-control mtl tasty
    tasty-expected-failure tasty-hunit tasty-th
  ];
  benchmarkHaskellDepends = [ async base criterion deepseq ];
  homepage = "https://github.com/maoe/lifted-async";
  description = "Run lifted IO operations asynchronously and wait for their results";
  license = stdenv.lib.licenses.bsd3;
}
