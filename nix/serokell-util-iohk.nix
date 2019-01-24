{ mkDerivation, aeson, ansi-terminal, base, base16-bytestring
, base64-bytestring, bytestring, clock, deepseq, exceptions, extra
, fetchgit, fmt, formatting, hashable, hspec, hspec-discover
, microlens, microlens-mtl, mtl, o-clock, parsec, process
, QuickCheck, quickcheck-instances, scientific, stdenv
, template-haskell, text, th-lift-instances, transformers
, universum, unordered-containers, vector
}:
mkDerivation {
  pname = "serokell-util";
  version = "0.9.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/serokell-util";
    sha256 = "16arrlxjkz9f8rd8v3l0yj70f2ij51didsxcz54jdv3j14pzmb5s";
    rev = "42586f5ea157b4a5411fbcbe41d1bf28bd942438";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson ansi-terminal base base16-bytestring base64-bytestring
    bytestring clock deepseq exceptions fmt formatting hashable
    microlens microlens-mtl mtl o-clock parsec process QuickCheck
    quickcheck-instances scientific template-haskell text
    th-lift-instances transformers universum unordered-containers
    vector
  ];
  testHaskellDepends = [
    aeson base extra hspec QuickCheck quickcheck-instances scientific
    universum unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/serokell/serokell-util";
  description = "General-purpose functions by Serokell";
  license = stdenv.lib.licenses.mit;
}
