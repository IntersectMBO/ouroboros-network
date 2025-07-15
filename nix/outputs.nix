{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib project utils ghc; };

  exposed-haskell-packages = {
    kes-agent-test = project.flake'.packages."kes-agent:test:kes-agent-tests";
    kes-agent = project.flake'.packages."kes-agent:exe:kes-agent";
    kes-service-client-demo = project.flake'.packages."kes-agent:exe:kes-service-client-demo";
    kes-agent-control = project.flake'.packages."kes-agent:exe:kes-agent-control";
  };

  static-haskell-packages = {
    # TODO: the pkgs.gitMinimal (needed for the kes-agent version command) at compile time gives an error for the musl64 build
    # musl64-kes-agent = project.projectCross.musl64.hsPkgs.kes-agent.components.exes.kes-agent;
  };

  packages =
    exposed-haskell-packages //
    static-haskell-packages;

  devShells = rec {
    default = ghc966;
    ghc966 = mkShell "ghc966";
    # ghc984 = mkShell "ghc984";
    # ghc9102 = mkShell "ghc9102";
    # ghc9122 = mkShell "ghc9122";
  };

  projectFlake = project.flake { };

  defaultHydraJobs = {
    ghc966 = projectFlake.hydraJobs.ghc966;
    # ghc984 = projectFlake.hydraJobs.ghc984;
    # ghc9102 = projectFlake.hydraJobs.ghc9102;
    # ghc9122 = projectFlake.hydraJobs.ghc9122;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
  };

  hydraJobsPerSystem = {
    "x86_64-linux" = defaultHydraJobs;
    "x86_64-darwin" = defaultHydraJobs;
    "aarch64-linux" = defaultHydraJobs;
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
in

{
  inherit packages;
  inherit devShells;
  inherit hydraJobs;
}
