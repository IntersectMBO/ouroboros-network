{ mkDerivation, ansi-terminal, array, base, call-stack, clock
, deepseq, directory, filepath, hspec-expectations, HUnit
, QuickCheck, quickcheck-io, random, setenv, stdenv, stm, time
, transformers
}:
mkDerivation {
  pname = "hspec-meta";
  version = "2.6.0";
  sha256 = "e6d701c9f366f6762eb2a86022d1c7a7d7631c100945491ff53b3a3e86212ad8";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm time transformers
  ];
  executableHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm time transformers
  ];
  homepage = "http://hspec.github.io/";
  description = "A version of Hspec which is used to test Hspec itself";
  license = stdenv.lib.licenses.mit;
}
