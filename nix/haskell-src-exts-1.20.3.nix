{ mkDerivation, array, base, containers, directory, filepath
, ghc-prim, happy, mtl, pretty, pretty-show, smallcheck, stdenv
, tasty, tasty-golden, tasty-smallcheck
}:
mkDerivation {
  pname = "haskell-src-exts";
  version = "1.20.3";
  sha256 = "433e68a731fb6a1435e86d3eb3b2878db9c5d51dc1f7499d85bbf5ac3ed1e4a8";
  libraryHaskellDepends = [ array base ghc-prim pretty ];
  libraryToolDepends = [ happy ];
  testHaskellDepends = [
    base containers directory filepath mtl pretty-show smallcheck tasty
    tasty-golden tasty-smallcheck
  ];
  doCheck = false;
  homepage = "https://github.com/haskell-suite/haskell-src-exts";
  description = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
  license = stdenv.lib.licenses.bsd3;
}
