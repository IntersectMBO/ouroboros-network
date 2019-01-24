{ mkDerivation, attoparsec, attoparsec-iso8601, base, base-compat
, bytestring, Cabal, cabal-doctest, containers, cookie, directory
, doctest, filepath, hashable, hspec, hspec-discover, http-types
, HUnit, nats, QuickCheck, quickcheck-instances, stdenv, tagged
, text, time, time-locale-compat, unordered-containers, uuid-types
}:
mkDerivation {
  pname = "http-api-data";
  version = "0.4";
  sha256 = "837e3f39f23df2caa23d75a4608f4a0505a1ab23f7290006976a37a373164a8a";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    attoparsec attoparsec-iso8601 base base-compat bytestring
    containers cookie hashable http-types tagged text time
    time-locale-compat unordered-containers uuid-types
  ];
  testHaskellDepends = [
    base base-compat bytestring cookie directory doctest filepath hspec
    HUnit nats QuickCheck quickcheck-instances text time
    unordered-containers uuid-types
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/fizruk/http-api-data";
  description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
  license = stdenv.lib.licenses.bsd3;
}
