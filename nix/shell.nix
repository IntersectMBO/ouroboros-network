{ hls, profiling, inputs, pkgs, ouroboros-network }:

let
  inherit (pkgs) lib;
  mkProfiling = pkg:
    pkg.appendModule {
      modules = [{
        enableProfiling = true;
        enableLibraryProfiling = true;
        # https://well-typed.com/blog/2023/03/prof-late/
        # note 'late' profiling is only available since `GHC-9.2`
        profilingDetail = "late";
      }];
    };

  hsPkgs =
    if profiling
    then mkProfiling ouroboros-network
    else ouroboros-network;
in
hsPkgs.shellFor {
  buildInputs = [
    pkgs.bashInteractive
  ];

  nativeBuildInputs = [
    pkgs.cabal
    pkgs.cabal-gild
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.stylish-haskell
    pkgs.ghcid
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        collection-latexextra
        collection-latexrecommended
        collection-mathscience
        latexmk;
    })
    # requirements of release scripts
    pkgs.jq
    pkgs.yq-go
    pkgs.gh
  ];

  # This is the place for tools that are required to be built with the same GHC
  # version as used in ouroboros-network.
  tools =
    lib.optionalAttrs hls
      {
        haskell-language-server = {
          src = inputs.haskellNix.inputs."hls-2.7";
          configureArgs = "--disable-benchmarks --disable-tests";
        };
      };

  shellHook = ''
    export SHELL=/run/current-system/sw/bin/bash
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}

