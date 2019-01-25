{ mkDerivation, array, base, bytestring, cereal, containers, lens
, lens-action, old-time, QuickCheck, quickcheck-instances, stdenv
, tasty, tasty-quickcheck, template-haskell, text, time, vector
}:
mkDerivation {
  pname = "safecopy";
  version = "0.9.4.3";
  sha256 = "787db1a56b7024ab5619b4f25af5379133f5f2a5e1a0657e66c8dfac1a131f08";
  libraryHaskellDepends = [
    array base bytestring cereal containers old-time template-haskell
    text time vector
  ];
  testHaskellDepends = [
    array base cereal containers lens lens-action QuickCheck
    quickcheck-instances tasty tasty-quickcheck template-haskell time
    vector
  ];
  homepage = "https://github.com/acid-state/safecopy";
  description = "Binary serialization with version control";
  license = stdenv.lib.licenses.publicDomain;
}
