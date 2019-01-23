{ mkDerivation, ansi-terminal, async, base, clock, containers, mtl
, optparse-applicative, stdenv, stm, tagged, unbounded-delays, unix
, wcwidth
}:
mkDerivation {
  pname = "tasty";
  version = "1.2";
  sha256 = "d6185e079ac9c12068582cc6f5b50d37a3d2d3ed1a05a4db454340350b5d8317";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers mtl optparse-applicative
    stm tagged unbounded-delays unix wcwidth
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
