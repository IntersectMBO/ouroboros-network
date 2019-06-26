{ mkDerivation, base, DeltaQIllustration, HaTeX, ihaskell
, ihaskell-hatex, stdenv
}:
mkDerivation {
  pname = "DeltaQIHaskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base DeltaQIllustration HaTeX ihaskell ihaskell-hatex
  ];
  description = "Interface module to isolate DeltaQ libraries from IHaskell/Jupyter requirements";
  license = stdenv.lib.licenses.bsd3;
}
