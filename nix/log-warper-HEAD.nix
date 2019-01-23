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
    sha256 = "1did5mw433pfig2p6s8napqml9r45x7vs4szj7hj7li05h3nz5qd";
    rev = "22a0967b33d70d42e441cc6d69829476c2d3d1b5";
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
