{ ... }@args:
#
# The defaul.nix file. This will generate targets for all
# buildables (see release.nix for nomenclature, excluding
# the "build machine" last part, specific to release.nix), eg.:
#
# - nix build -f default.nix nix-tools.tests.io-sim # All `io-sim` tests
# - nix build -f default.nix nix-tools.tests.ouroboros-consensus.test-consensus
#
# Generated targets include anything from stack.yaml (via
# nix-tools:stack-to-nix and the nix/regenerate.sh script)
# or cabal.project (via nix-tools:plan-to-nix), including all
# version overrides specified there.
#
# Nix-tools stack-to-nix will generate the `nix/.stack-pkgs.nix`
# file which is imported from the `nix/pkgs.nix` where further
# customizations outside of the ones in stack.yaml/cabal.project
# can be specified as needed for nix/ci.
#
# Please run `nix/regenerate.sh` after modifying stack.yaml
# or relevant part of cabal configuration files.
# When switching to recent stackage or hackage package version,
# you might also need to update the iohk-nix common lib. You
# can do so by running the `nix/update-iohk-nix.sh` script.
#
# More information about iohk-nix and nix-tools is available at:
# https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix-toolification.org#for-a-stackage-project
#

# We will need to import the iohk-nix common lib, which includes
# the nix-tools tooling.
let
  commonLib = import ./nix/iohk-common.nix;

# This file needs to export a function that takes
# the arguments it is passed and forwards them to
# the default-nix template from iohk-nix. This is
# important so that the release.nix file can properly
# parameterize this file when targetting different
# hosts.
  nixTools = commonLib.nix-tools.default-nix ./nix/pkgs.nix args;
  # TODO: remove when proxy and old cardano configs no
  # longer required.
  oldCardanoSrc = import ./nix/old-cardano.nix {
    inherit commonLib;
  };
  byronProxy = nixTools.nix-tools.exes.byron-proxy;
  # TODO: move scripts to cardano-shell at some point.
  byronProxyScripts = import ./nix/byron-proxy-scripts.nix {
    inherit commonLib oldCardanoSrc byronProxy;
  };
  tests = import ./nix/nixos/tests { pkgs = commonLib.pkgs; };
in {
  inherit byronProxyScripts tests;
  inherit (nixTools) nix-tools;
}
