{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib project utils ghc; };

  packages = { };

  devShells = rec {
    default = ghc966; 
    ghc966 = mkShell "ghc966"; 
    ghc984 = mkShell "ghc984"; 
    ghc9102 = mkShell "ghc9102"; 
    ghc9122 = mkShell "ghc9122"; 
  };

  projectFlake = project.flake {};

  defaultHydraJobs = { 
    ghc966 = projectFlake.hydraJobs.ghc966;
    ghc984 = projectFlake.hydraJobs.ghc984;
    ghc9102 = projectFlake.hydraJobs.ghc9102;
    ghc9122 = projectFlake.hydraJobs.ghc9122;
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
