{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fetchgit
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, th-lift, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/avieth/haskell-hedgehog.git";
    sha256 = "1r4i41s8rhj38kx5kvj2fwj1a3kiwhw1wiyww9q0ivpb8ifhkbqw";
    rev = "d965a9002b16b82a548da4c071b0eb0af8ed7838";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/hedgehog; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text th-lift time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
