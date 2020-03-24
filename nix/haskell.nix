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
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit { src = ../.; };
    ghc = buildPackages.haskell-nix.compiler.${compiler};
    modules = [

      # Allow reinstallation of Win32
      { nonReinstallablePkgs =
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
      }
      {
        packages.katip.components.library.doExactConfig = true;
        packages.typed-protocols.configureFlags = [ "--ghc-option=-Werror" ];
        packages.typed-protocols-examples.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim-classes.configureFlags = [ "--ghc-option=-Werror" ];
        packages.Win32-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.network-mux.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ntp-client.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network-framework.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-byron.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-byronspec.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-cardano.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-mock.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-shelley.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-consensus-test-infra.configureFlags = [ "--ghc-option=-Werror" ];
        packages.prometheus.components.library.doExactConfig = true;
        enableLibraryProfiling = profiling;
      }
      (if stdenv.hostPlatform.isWindows then {
        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";

        # ruby/perl dependencies cannot be cross-built for cddl tests:
        packages.ouroboros-network.flags.cddl = false;

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      } else {
        packages.ouroboros-network.flags.cddl = true;
        packages.ouroboros-network.components.tests.test-cddl.build-tools = [pkgs.cddl pkgs.cbor-diag];
      })
    ];
    configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  };
in
  pkgSet
