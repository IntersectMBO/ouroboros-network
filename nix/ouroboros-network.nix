# ###########################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib, stdenv, pkgs, haskell-nix, CHaP, buildPackages, config ? { }
  # Enable profiling
, profiling ? config.haskellNix.profiling or false
, libsodium-vrf ? pkgs.libsodium-vrf, secp256k1 ? pkgs.secp256k1
  # Enable strict TVar invariant check flag in strict-stm
, checkTVarInvariant ? false }:
let
  compiler-nix-name = pkgs.localConfig.ghcVersion;
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };
  inputMap = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject { inherit compiler-nix-name src inputMap; }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject [
    ({ lib, pkgs, buildProject, ... }: {
      options = {
        coverage = lib.mkOption {
          type = lib.types.bool;
          description =
            "Enable Haskell Program Coverage for ouroboros-network libraries and test suites.";
          default = false;
        };
      };
    })
    ({ config, ... }: {
      inherit compiler-nix-name src inputMap;
      modules = [

        {
          # Compile all local packages with -Werror:
          packages = lib.genAttrs projectPackages
            (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
        }
        {
          packages = lib.genAttrs projectPackages (name: {
            # Enable Haskell Program Coverage for all local libraries
            # and test suites.
            doCoverage = config.coverage;
          });
        }
        {
          packages.strict-stm.components.library.configureFlags = lib.mkForce
            (if checkTVarInvariant then [ "-f checktvarinvariant" ] else [ ]);
        }
        ({ pkgs, ... }: {
          # Apply profiling arg to all library components in the build:
          enableLibraryProfiling = profiling;
        })

        # Options specific to the windows cross-compiled build:
        ({ pkgs, ... }:
          lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
            # ruby/perl dependencies cannot be cross-built for cddl tests:
            packages.ouroboros-network-protocols.flags.cddl = false;

            # Make sure we use a buildPackages version of happy
            packages.pretty-show.components.library.build-tools =
              [ buildPackages.haskell-nix.haskellPackages.happy ];

            # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
            packages.Win32.components.library.build-tools = lib.mkForce [ ];
            packages.terminal-size.components.library.build-tools =
              lib.mkForce [ ];
            packages.network.components.library.build-tools = lib.mkForce [ ];
          })
        # Options for when not compiling to windows:
        ({ pkgs, ... }:
          lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
            packages.ouroboros-network-protocols.flags.cddl = true;
            packages.ouroboros-network-protocols.components.tests.cddl.build-tools =
              [ pkgs.cddl pkgs.cbor-diag ];
            packages.ouroboros-network-protocols.components.tests.cddl.preCheck =
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
    })
  ];
in pkgSet
