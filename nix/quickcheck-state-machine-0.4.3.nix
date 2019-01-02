{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, directory, doctest, exceptions, filelock, filepath, http-client
, lifted-async, matrix, monad-control, monad-logger, mtl, network
, persistent, persistent-postgresql, persistent-template
, pretty-show, process, QuickCheck, quickcheck-instances, random
, resourcet, servant, servant-client, servant-server, split, stdenv
, stm, strict, string-conversions, tasty, tasty-hunit
, tasty-quickcheck, text, tree-diff, vector, wai, warp
}:
mkDerivation {
  pname = "quickcheck-state-machine";
  version = "0.4.3";
  sha256 = "3d2b858e43764da5b9d390048e43ef91a4128b75a1921a8a7f68299ba1d43039";
  libraryHaskellDepends = [
    ansi-wl-pprint base containers exceptions lifted-async matrix
    monad-control mtl pretty-show QuickCheck split stm tree-diff vector
  ];
  testHaskellDepends = [
    base bytestring directory doctest filelock filepath http-client
    lifted-async matrix monad-control monad-logger mtl network
    persistent persistent-postgresql persistent-template process
    QuickCheck quickcheck-instances random resourcet servant
    servant-client servant-server stm strict string-conversions tasty
    tasty-hunit tasty-quickcheck text tree-diff vector wai warp
  ];
  homepage = "https://github.com/advancedtelematic/quickcheck-state-machine#readme";
  description = "Test monadic programs using state machine based models";
  license = stdenv.lib.licenses.bsd3;
}
