{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, directory, doctest, exceptions, fetchgit, filelock, filepath
, http-client, matrix, monad-logger, mtl, network, persistent
, persistent-postgresql, persistent-template, pretty-show, process
, QuickCheck, quickcheck-instances, random, resourcet, servant
, servant-client, servant-server, stdenv, strict
, string-conversions, tasty, tasty-hunit, tasty-quickcheck, text
, tree-diff, unliftio, vector, wai, warp
}:
mkDerivation {
  pname = "quickcheck-state-machine";
  version = "0.6.0";
  src = fetchgit {
    url = "https://github.com/advancedtelematic/quickcheck-state-machine";
    sha256 = "0dn45kzx4hgb5bhj03nvx74jqk2a0rnj894gvqrxad3f7zd05y0f";
    rev = "619620343ad2c862c5855166c815c478a619b76f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    ansi-wl-pprint base containers exceptions matrix mtl pretty-show
    QuickCheck tree-diff unliftio vector
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest filelock filepath
    http-client matrix monad-logger mtl network persistent
    persistent-postgresql persistent-template pretty-show process
    QuickCheck quickcheck-instances random resourcet servant
    servant-client servant-server strict string-conversions tasty
    tasty-hunit tasty-quickcheck text tree-diff unliftio vector wai
    warp
  ];
  homepage = "https://github.com/advancedtelematic/quickcheck-state-machine#readme";
  description = "Test monadic programs using state machine based models";
  license = stdenv.lib.licenses.bsd3;
}
