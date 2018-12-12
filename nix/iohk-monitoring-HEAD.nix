{ mkDerivation, aeson, array, async, auto-update, base, bytestring
, clock, containers, contravariant, directory, download, ekg
, ekg-core, fetchgit, filepath, katip, lens, mtl, process
, QuickCheck, random, safe-exceptions, scientific, semigroups
, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, time, time-units, transformers, unix
, unordered-containers, vector, void, yaml
}:
mkDerivation {
  pname = "iohk-monitoring";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/iohk-monitoring-framework";
    sha256 = "15k08x3si048his1gf2g6rmqp78szyxj4z3z8fbkcp5mjbwzngln";
    rev = "95a19ed4b01522793b71968b047c69d832b5663e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async auto-update base bytestring clock containers
    contravariant directory ekg ekg-core filepath katip lens mtl
    safe-exceptions scientific stm template-haskell text time
    time-units transformers unix unordered-containers vector yaml
  ];
  executableHaskellDepends = [
    async base bytestring download mtl random text unix
  ];
  testHaskellDepends = [
    aeson array async base bytestring clock containers directory mtl
    process QuickCheck random semigroups stm tasty tasty-hunit
    tasty-quickcheck text time time-units transformers
    unordered-containers vector void yaml
  ];
  description = "logging, benchmarking and monitoring framework";
  license = stdenv.lib.licenses.mit;
}
