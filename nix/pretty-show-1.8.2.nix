{ mkDerivation, array, base, filepath, ghc-prim, happy
, haskell-lexer, pretty, stdenv, text
}:
mkDerivation {
  pname = "pretty-show";
  version = "1.8.2";
  sha256 = "2aa4c750d7ca7f7319e02472098381db4780358f6ddd56f398171e94671884b9";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base filepath ghc-prim haskell-lexer pretty text
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [ base ];
  homepage = "http://wiki.github.com/yav/pretty-show";
  description = "Tools for working with derived `Show` instances and generic inspection of values";
  license = stdenv.lib.licenses.mit;
}
