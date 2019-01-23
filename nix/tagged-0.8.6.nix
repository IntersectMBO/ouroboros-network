{ mkDerivation, base, deepseq, stdenv, template-haskell
, transformers
}:
mkDerivation {
  pname = "tagged";
  version = "0.8.6";
  sha256 = "ad16def0884cf6f05ae1ae8e90192cf9d8d9673fa264b249499bd9e4fac791dd";
  libraryHaskellDepends = [
    base deepseq template-haskell transformers
  ];
  homepage = "http://github.com/ekmett/tagged";
  description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
  license = stdenv.lib.licenses.bsd3;
}
