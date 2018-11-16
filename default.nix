let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}  # The nixpkgs configuration file
# Use a pinned version nixpkgs.
, pkgs ? localLib.pkgs

# Disable running of tests for all local packages.
, forceDontCheck ? false

# Enable profiling for all haskell packages.
# Profiling slows down performance by 50% so we don't enable it by default.
, enableProfiling ? false

# Enable separation of build/check derivations.
, enableSplitCheck ? true

# Keeps the debug information for all haskell packages.
, enableDebugging ? false

# Build (but don't run) benchmarks for all local packages.
, enableBenchmarks ? false

# Overrides all nix derivations to add build timing information in
# their build output.
, enablePhaseMetrics ? true

# Overrides all nix derivations to add haddock hydra output.
, enableHaddockHydra ? false

# Disables optimization in the build for all local packages.
, fasterBuild ? false
, compiler   ? "ghc862"
}:

let
  packages = self: ({
    inherit pkgs;
    haskellPackages = self.callPackage localLib.iohkNix.haskellPackages {
      inherit forceDontCheck enableProfiling enablePhaseMetrics enableHaddockHydra
        enableBenchmarks fasterBuild enableDebugging enableSplitCheck;
      pkgsGenerated = pkgs.haskell.packages.${compiler};
      ghc = pkgs.haskell.compiler.${compiler};
      filter = localLib.isLocalPackage;
      requiredOverlay = ./nix/overlays/required.nix;
    };
  }
  # fixme: Temporary fix for hydra evaluation
  // { inherit (self.haskellPackages)
       ouroboros-network;
     });
in pkgs.lib.makeScope pkgs.newScope packages
