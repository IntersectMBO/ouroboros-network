{ mkDerivation, base, base-compat, contravariant, hspec
, hspec-discover, QuickCheck, stdenv
}:
mkDerivation {
  pname = "base-compat-batteries";
  version = "0.10.5";
  sha256 = "175dcfd1453bd02ec955c05181cbf4278af145183b5899c62d3be29d866170ee";
  libraryHaskellDepends = [ base base-compat contravariant ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  description = "base-compat with extra batteries";
  license = stdenv.lib.licenses.mit;
}
