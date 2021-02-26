############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8104"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
, libsodium ? pkgs.libsodium
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "ouroboros-network-src";
      src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [

      # Compile all local packages with -Werror:
      {
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      {
        enableLibraryProfiling = profiling;
      }
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        # Allow reinstallation of Win32
        nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim" "ghcjs-th"
            "ghc-boot"
            "ghc" "array" "binary" "bytestring" "containers"
            "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl" "parsec" "text" "transformers"
            "xhtml"
            # "stm" "terminfo"
          ];
        # ruby/perl dependencies cannot be cross-built for cddl tests:
        packages.ouroboros-network.flags.cddl = false;

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];

        # Make sure that libsodium DLLs are available for tests
        packages.ouroboros-consensus-byron-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-cardano-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-mock-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-shelley-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-test.components.tests.test-consensus.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-test.components.tests.test-infra.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.ouroboros-consensus-test.components.tests.test-storage.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
      })
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
        packages.ouroboros-network.flags.cddl = true;
        packages.ouroboros-network.components.tests.cddl.build-tools = [pkgs.cddl pkgs.cbor-diag];
        packages.ouroboros-network.components.tests.cddl.preCheck = "export HOME=`pwd`";
      })
    ];
    configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  };
in
  pkgSet
