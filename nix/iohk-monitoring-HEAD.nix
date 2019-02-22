{ mkDerivation, aeson, array, async, attoparsec, auto-update, base
, bytestring, clock, containers, contravariant, directory, download
, ekg, ekg-core, fetchgit, filepath, katip, lens, libyaml, mtl
, process, QuickCheck, random, safe-exceptions, scientific
, semigroups, split, stdenv, stm, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, time, time-units
, transformers, unix, unordered-containers, vector, void, yaml
}:
mkDerivation {
  pname = "iohk-monitoring";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/iohk-monitoring-framework.git";
    sha256 = "0926bk8jvprc69csiav0kfv984nn1lq840gr1nj58i1xfdhi8wmw";
    rev = "34f3b7febef064856fc27df9f36626dd186029ed";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async attoparsec auto-update base bytestring clock
    containers contravariant directory ekg ekg-core filepath katip lens
    libyaml mtl safe-exceptions scientific stm template-haskell text
    time time-units transformers unix unordered-containers vector yaml
  ];
  executableHaskellDepends = [
    async base bytestring download mtl random text unix
  ];
  testHaskellDepends = [
    aeson array async base bytestring clock containers directory
    filepath libyaml mtl process QuickCheck random semigroups split stm
    tasty tasty-hunit tasty-quickcheck text time time-units
    transformers unordered-containers vector void yaml
  ];
  description = "logging, benchmarking and monitoring framework";
  license = stdenv.lib.licenses.mit;
}
