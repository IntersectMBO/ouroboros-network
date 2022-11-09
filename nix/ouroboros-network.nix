# ###########################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib, stdenv, pkgs, haskell-nix, buildPackages, config ? { }
  # Enable profiling
, profiling ? config.haskellNix.profiling or false
, libsodium-vrf ? pkgs.libsodium-vrf, secp256k1 ? pkgs.secp256k1 }:
let
  compiler-nix-name = pkgs.localConfig.ghcVersion;
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject { inherit compiler-nix-name src; }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit compiler-nix-name src;
    modules = [

      {
        # Compile all local packages with -Werror:
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      {
        # Apply profiling arg to all library components in the build:
        enableLibraryProfiling = profiling;

        # Command-line options for test suites:
        packages.ouroboros-consensus-byron-test.components.tests.test.testFlags =
          lib.mkForce [ "--no-create" ];
        packages.ouroboros-consensus-shelley-test.components.tests.test.testFlags =
          lib.mkForce [ "--no-create" ];
        packages.ouroboros-consensus-cardano-test.components.tests.test.testFlags =
          lib.mkForce [ "--no-create" ];

        packages.cardano-crypto-praos.components.library.pkgconfig =
          lib.mkForce [ [ libsodium-vrf ] ];
        packages.cardano-crypto-class.components.library.pkgconfig =
          lib.mkForce [[ libsodium-vrf secp256k1 ]];
      }

      # Options specific to the windows cross-compiled build:
      ({ pkgs, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
          # Allow reinstallation of Win32
          nonReinstallablePkgs = [
            "rts"
            "ghc-heap"
            "ghc-prim"
            "integer-gmp"
            "integer-simple"
            "base"
            "deepseq"
            "array"
            "ghc-boot-th"
            "pretty"
            "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim"
            "ghcjs-th"
            "ghc-boot"
            "ghc"
            "array"
            "binary"
            "bytestring"
            "containers"
            "filepath"
            "ghc-boot"
            "ghc-compact"
            "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl"
            "parsec"
            "text"
            "transformers"
            "xhtml"
            # "stm" "terminfo"
          ];
          # ruby/perl dependencies cannot be cross-built for cddl tests:
          packages.ouroboros-network.flags.cddl = false;

          # Make sure we use a buildPackages version of happy
          packages.pretty-show.components.library.build-tools =
            [ buildPackages.haskell-nix.haskellPackages.happy ];

          # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
          packages.Win32.components.library.build-tools = lib.mkForce [ ];
          packages.terminal-size.components.library.build-tools =
            lib.mkForce [ ];
          packages.network.components.library.build-tools = lib.mkForce [ ];

          # Make sure that libsodium DLLs are available for tests
          packages.ouroboros-consensus-byron-test.components.tests.test.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-cardano-test.components.tests.test.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-mock-test.components.tests.test.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-shelley-test.components.tests.test.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-test.components.tests.test-consensus.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-test.components.tests.test-infra.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-test.components.tests.test-storage.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
          packages.ouroboros-consensus-cardano-tools.components.tests.test.postInstall =
            ''
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.secp256k1}/bin/libsecp256k1-0.dll $out/bin/libsecp256k1-0.dll
            '';
        })
      # Options for when not compiling to windows:
      ({ pkgs, ... }:
        lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
          packages.ouroboros-network.flags.cddl = true;
          packages.ouroboros-network.components.tests.cddl.build-tools =
            [ pkgs.cddl pkgs.cbor-diag ];
          packages.ouroboros-network.components.tests.cddl.preCheck =
            "export HOME=`pwd`";
        })
      # Fix for Plutus compilation with profiling
      ({ pkgs, ... }:
        lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
          # Needed for profiled builds to fix an issue loading recursion-schemes part of makeBaseFunctor
          # that is missing from the `_p` output.  See https://gitlab.haskell.org/ghc/ghc/-/issues/18320
          # This work around currently breaks regular builds on macOS with:
          # <no location info>: error: ghc: ghc-iserv terminated (-11)
          packages.plutus-core.components.library.ghcOptions =
            [ "-fexternal-interpreter" ];
        })
    ];
  };
in pkgSet
