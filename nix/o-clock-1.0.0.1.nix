{ mkDerivation, base, deepseq, doctest, gauge, ghc-prim, Glob
, hedgehog, markdown-unlit, stdenv, tasty, tasty-hedgehog
, tasty-hspec, tiempo, time-units, type-spec
}:
mkDerivation {
  pname = "o-clock";
  version = "1.0.0.1";
  sha256 = "d9198d86927002eb7fb9c9ba52afd965719d3a6a1d19818f188d8b06f707bb5a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ghc-prim ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest Glob hedgehog markdown-unlit tasty tasty-hedgehog
    tasty-hspec type-spec
  ];
  testToolDepends = [ doctest markdown-unlit ];
  benchmarkHaskellDepends = [ base deepseq gauge tiempo time-units ];
  homepage = "https://github.com/serokell/o-clock";
  description = "Type-safe time library";
  license = stdenv.lib.licenses.mit;
}
