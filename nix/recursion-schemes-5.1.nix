{ mkDerivation, base, base-orphans, comonad, free, HUnit, stdenv
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "recursion-schemes";
  version = "5.1";
  sha256 = "01db11b8eb64b11a9f2b65a4d5422dee351b8991aa3ae04c91a2ed016745f3d2";
  libraryHaskellDepends = [
    base base-orphans comonad free template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base HUnit template-haskell transformers ];
  homepage = "http://github.com/ekmett/recursion-schemes/";
  description = "Generalized bananas, lenses and barbed wire";
  license = stdenv.lib.licenses.bsd3;
}
