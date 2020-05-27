############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ ouroboros-network ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    inherit sourcesOverride;
    config = { allowUnfree = false; inHydra = true; };
    gitrev = ouroboros-network.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

, withProblematicWindowsTests ? false

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import ouroboros-network;
  gitrev = ouroboros-network.rev;
};

with pkgs.lib;

let
  # restrict supported systems to a subset where tests (if exist) are required to pass:
  testsSupportedSystems = intersectLists supportedSystems [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  disabledMingwW64Tests = recursiveUpdate (if withProblematicWindowsTests then {} else {
    haskellPackages.ouroboros-network.checks.test-network = null;
    checks.tests.ouroboros-network.test-network = null;
    haskellPackages.Win32-network.checks.test-Win32-network = null;
    checks.tests.Win32-network.test-Win32-network = null;
    haskellPackages.network-mux.checks.test-network-mux = null;
    checks.tests.network-mux.test-network-mux = null;
    haskellPackages.ouroboros-network-framework.checks.ouroboros-network-framework-tests = null;
    checks.tests.ouroboros-network-framework.ouroboros-network-framework-tests = null;
  }) {
    haskellPackages.ouroboros-network.components.tests.test-cddl = null;
    tests.ouroboros-network.test-cddl = null;
    haskellPackages.ouroboros-network.checks.test-cddl = null;
    checks.tests.ouroboros-network.test-cddl = null;
  };

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    "${mingwW64.config}" = recursiveUpdate (mapTestOnCross mingwW64 (packagePlatformsCross project)) disabledMingwW64Tests;
    # TODO: fix broken evals
    #musl64 = mapTestOnCross musl64 (packagePlatformsCross project);
  } // (mkRequiredJob (concatLists [
    (collectJobs jobs."${mingwW64.config}".checks.tests)
    (collectJobs jobs.native.checks)
    (collectJobs jobs.native.benchmarks)
    (collectJobs jobs.native.libs)
    (collectJobs jobs.native.exes)
  ])) // {
    # This is used for testing the build on windows.
    ouroboros-network-tests-win64 = pkgs.callPackage ./nix/windows-testing-bundle.nix {
      inherit project;
      tests = collectJobs jobs."${mingwW64.config}".tests;
      benchmarks = collectJobs jobs."${mingwW64.config}".benchmarks;
    };
  };

in jobs
