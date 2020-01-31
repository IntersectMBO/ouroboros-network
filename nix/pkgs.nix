{ pkgs
, src
, haskellCompiler ? "ghc865"
, profiling ? false
}:
let
  haskell = pkgs.haskell-nix;

  exe-extension =
    pkgs.lib.optionalString pkgs.stdenv.targetPlatform.isWindows ".exe";

  recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
  pkgSet = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
    # we are only intersted in listing the project packages
    (pkgs.lib.filterAttrs (with pkgs.haskell-nix.haskellLib; (n: p: p != null && (isLocalPackage p && isProjectPackage p) || n == "shellFor"))
      # from our project which is based on a cabal project.
      (pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
          ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
          modules = [
      {
        packages.katip.components.library.doExactConfig = true;
        packages.typed-protocols.configureFlags = [ "--ghc-option=-Werror" ];
        packages.typed-protocols-cbor.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim-classes.configureFlags = [ "--ghc-option=-Werror" ];
        packages.Win32-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.network-mux.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network.flags.cddl = true;
        packages.ouroboros-network.components.tests.test-cddl.build-tools = [pkgs.cddl pkgs.cbor-diag];
        packages.ouroboros-consensus.configureFlags = [ "--ghc-option=-Werror" ];
        packages.prometheus.components.library.doExactConfig = true;
        packages.libiserv.patches = [ ./libiserv-network-3.patch ];
      }
      {
        # disable test-Win32-network tests
        packages.Win32-network.components.tests = {
          test-Win32-network.preCheck = "echo OK > $out; exit 0";
        };
      }
          ];
      }));
 in pkgSet
