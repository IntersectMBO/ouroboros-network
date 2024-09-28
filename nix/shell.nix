{ hls, inputs, pkgs, ouroboros-network }:

let
  inherit (pkgs) lib;
in
ouroboros-network.shellFor {
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
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}

