{ mkDerivation, base, containers, deepseq, erf, process, random
, stdenv, template-haskell, tf-random, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.12.4";
  sha256 = "c9f6b226b9f890ddf1506d9993c6b490aee66b598761e054f0de2a21b5ec4f5d";
  libraryHaskellDepends = [
    base containers deepseq erf random template-haskell tf-random
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
