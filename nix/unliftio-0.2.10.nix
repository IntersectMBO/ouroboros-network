{ mkDerivation, async, base, containers, deepseq, directory
, filepath, gauge, hspec, process, QuickCheck, stdenv, stm, time
, transformers, unix, unliftio-core
}:
mkDerivation {
  pname = "unliftio";
  version = "0.2.10";
  sha256 = "141d6e858f3c340c881d9853a38076ca09306e45a02fffc36885b9ee11cf1b5c";
  libraryHaskellDepends = [
    async base deepseq directory filepath process stm time transformers
    unix unliftio-core
  ];
  testHaskellDepends = [
    async base containers deepseq directory filepath hspec process
    QuickCheck stm time transformers unix unliftio-core
  ];
  benchmarkHaskellDepends = [
    async base deepseq directory filepath gauge process stm time
    transformers unix unliftio-core
  ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)";
  license = stdenv.lib.licenses.mit;
}
