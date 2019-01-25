{ mkDerivation, aeson, attoparsec, attoparsec-iso8601, base
, base-compat, base-orphans, bytestring, containers, exceptions
, hashable, QuickCheck, quickcheck-instances, scientific, stdenv
, tagged, tasty, tasty-hunit, tasty-quickcheck, text, time
, time-locale-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-compat";
  version = "0.3.9";
  sha256 = "e043941ba761c13a3854fc087521b864b56b2df874154e42aedb67b2a77f23c8";
  libraryHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base base-compat bytestring
    containers exceptions hashable scientific tagged text time
    time-locale-compat unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat base-orphans bytestring
    containers exceptions hashable QuickCheck quickcheck-instances
    scientific tagged tasty tasty-hunit tasty-quickcheck text time
    time-locale-compat unordered-containers vector
  ];
  homepage = "https://github.com/phadej/aeson-compat#readme";
  description = "Compatibility layer for aeson";
  license = stdenv.lib.licenses.bsd3;
}
