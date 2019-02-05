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
    sha256 = "11vw6h3lshhwrjbxni6z0jr6w9x2x338rv6p2b4b0rgr650pv2a9";
    rev = "16246d4fbf16da7984f2a4b6c42f2ed5098182e4";
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
