{
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;
  };

  outputs = inputs:
    let # all platforms on which we build
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        pkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = [
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
            (import ./nix/tools.nix inputs)
            (import ./nix/ouroboros-network.nix inputs)
            (import ./nix/network-docs.nix inputs)
          ];
        };

        buildSystem = pkgs.buildPlatform.system;
        flake = pkgs.ouroboros-network.flake { };
        format = pkgs.callPackage ./nix/formatting.nix pkgs;
        inherit (pkgs) lib network-docs;

        # shells accessible through `nix develop`,
        # `nix develop .\#devShells.x86_64-linux.ghc810`, etc.
        devShells = rec {
          default = import ./nix/shell.nix {
            hls = true;
            profiling = false;
            inherit inputs pkgs;
            ouroboros-network = pkgs.ouroboros-network;
          };
          profiling = import ./nix/shell.nix {
            hls = true;
            profiling = true;
            inherit inputs pkgs;
            ouroboros-network = pkgs.ouroboros-network;
          };
        };

        # jobs executed on hydra
        hydraJobs =
          pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even
                  # if no change other than commit hash)
                  revision = pkgs.writeText "revision" (inputs.self.rev or "dirty");
                }
                // lib.optionalAttrs (system == "x86_64-linux") {
                  devShell = devShells.default;
                  inherit format network-docs;
                };
            };

        # Provide hydraJobs through legacyPackages to allow building without system prefix, e.g.
        # `nix build .\#network-mux:lib:network-mux`
        # `nix build .\#network-docs`
        legacyPackages = { inherit hydraJobs network-docs format; };
      in
      lib.recursiveUpdate flake rec {
        project = pkgs.ouroboros-network;
        inherit hydraJobs legacyPackages devShells;
      }
    );
}
