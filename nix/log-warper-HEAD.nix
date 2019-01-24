{ mkDerivation, aeson, ansi-terminal, base, containers, deepseq
, directory, fetchgit, filepath, fmt, lifted-async
, microlens-platform, mmorph, monad-control, monad-loops, mtl
, o-clock, stdenv, text, time, transformers, transformers-base
, universum, unix, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "log-warper";
  version = "1.8.10.1";
  src = fetchgit {
    url = "https://github.com/input-output-hk/log-warper";
    sha256 = "06fim002611p97f8dgd3ifzpaw3qqv0n25qqm79n2m8x9hb1cdxz";
    rev = "717c48f7ba541981f05ef9d5f930912e45a2ca38";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson ansi-terminal base containers deepseq directory filepath fmt
    lifted-async microlens-platform mmorph monad-control monad-loops
    mtl o-clock text time transformers transformers-base universum unix
    unordered-containers vector yaml
  ];
  homepage = "https://github.com/serokell/log-warper";
  description = "Flexible, configurable, monadic and pretty logging";
  license = stdenv.lib.licenses.mit;
}
