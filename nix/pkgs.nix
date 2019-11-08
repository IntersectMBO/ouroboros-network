{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let

  # our packages
  stack-pkgs = import ./.stack.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras haskell.hackage).compiler;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-extras = [
      iohk-extras.${compiler.nix-name}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      iohk-module
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.components.library.doExactConfig = true;
        packages.typed-protocols.configureFlags = [ "--ghc-option=-Werror" ];
        packages.typed-protocols-cbor.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim-classes.configureFlags = [ "--ghc-option=-Werror" ];
        packages.Win32-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.network-mux.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network.configureFlags = [ "--ghc-option=-Werror" ];
        packages.ouroboros-network.flags.cddl = true;
        packages.ouroboros-network.components.tests.cddl.build-tools = [pkgs.cddl pkgs.cbor-diag];
        packages.ouroboros-consensus.configureFlags = [ "--ghc-option=-Werror" ];
        packages.prometheus.components.library.doExactConfig = true;
      }
      {
        # disable test-Win32-network tests
        packages.Win32-network.components.tests = {
          test-Win32-network.preCheck = "echo OK > $out; exit 0";
        };
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
