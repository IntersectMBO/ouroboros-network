{ mkDerivation, ansi-terminal, async, base, clock, containers, mtl
, optparse-applicative, stdenv, stm, tagged, unbounded-delays, unix
, wcwidth
}:
mkDerivation {
  pname = "tasty";
  version = "1.1.0.4";
  sha256 = "3b4e3fa2c7dce8452c2636e5fe22323919461f52e905c132aae8dc12f10beebf";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers mtl optparse-applicative
    stm tagged unbounded-delays unix wcwidth
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
