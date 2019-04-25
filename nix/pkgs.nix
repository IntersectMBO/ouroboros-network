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
        packages.ouroboros-network.configureFlags = [ "--ghc-option=-Werror" ];
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
