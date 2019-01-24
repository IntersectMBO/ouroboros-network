{ mkDerivation, base, bytestring, containers, deepseq, doctest
, fetchgit, fmt, formatting, gauge, ghc-prim, Glob, hashable
, hedgehog, microlens, microlens-mtl, mtl, safe-exceptions, stdenv
, stm, tasty, tasty-hedgehog, text, transformers, type-operators
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "universum";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/universum";
    sha256 = "12ppiszywj0dsspwlhb8bzhsrlgszk8rvlhcy8il3ppz99mlnw5g";
    rev = "7f1b2483f71cacdfd032fe447064d6e0a1df50fc";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers deepseq fmt formatting ghc-prim hashable
    microlens microlens-mtl mtl safe-exceptions stm text transformers
    type-operators unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog tasty tasty-hedgehog text
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base containers gauge unordered-containers
  ];
  homepage = "https://github.com/serokell/universum";
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
}
