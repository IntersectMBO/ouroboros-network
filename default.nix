{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-ledger --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages ouroborosNetworkHaskellPackages);

  validate-mainnet = import ./nix/validate-mainnet.nix {
    inherit pkgs;
    byron-db-converter = haskellPackages.ouroboros-consensus-byron.components.exes.db-converter;
    onlyImmDB = false;
  };

  self = {
    inherit haskellPackages;

    inherit (haskellPackages.ouroboros-network.identifier) version;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    # These are not run on hydra, but will be built separately in a nightly
    # build job.
    nightly-checks = {
      inherit validate-mainnet;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in self
