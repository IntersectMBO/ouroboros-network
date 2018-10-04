{ mkDerivation, base, optparse-applicative, pcre-light, QuickCheck
, random, stdenv, tagged, tasty, tasty-hunit
}:
mkDerivation {
  pname = "tasty-quickcheck";
  version = "0.10";
  sha256 = "10fd30cef4a0c2cefb70afecef5adcee1f32f0fd287f108321458fbfd6d7266f";
  revision = "1";
  editedCabalFile = "1ndkkywcqgb2wj339vgckjv5915da5kd4ixlkaww9fsba3qsrnwx";
  libraryHaskellDepends = [
    base optparse-applicative QuickCheck random tagged tasty
  ];
  testHaskellDepends = [ base pcre-light tasty tasty-hunit ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "QuickCheck support for the Tasty test framework";
  license = stdenv.lib.licenses.mit;
}
