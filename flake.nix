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

        flake = pkgs.ouroboros-network.flake {};
        check-stylish = pkgs.callPackage ./nix/check-stylish.nix { };
        inherit (pkgs) lib network-docs;

        # shells accessible through `nix develop`,
        # `nix develop .\#devShells.x86_64-linux.ghc810`, etc.
        devShells = rec {
          default  = import ./nix/shell.nix { hls = true;
                                              inherit inputs pkgs;
                                              ouroboros-network = pkgs.ouroboros-network; };
          profiled = import ./nix/shell.nix { hls = true;
                                              inherit inputs pkgs;
                                              ouroboros-network = pkgs.ouroboros-network.projectVariants.profiled; };
          ghc810   = import ./nix/shell.nix { hls = false; # hls-2.7.0.0 cannot be compiled for `ghc-8.10`
                                              inherit inputs pkgs;
                                              ouroboros-network = pkgs.ouroboros-network.projectVariants.ghc810; };
          ghc810-profiled
                   = import ./nix/shell.nix { hls = false;
                                              inherit inputs pkgs;
                                              ouroboros-network = pkgs.ouroboros-network.projectVariants.ghc810-profiled; };
        };

        # jobs executed on hydra
        hydraJobs =
          pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = pkgs.writeText "revision" (inputs.self.rev or "dirty");
                  inherit network-docs check-stylish;
                  devShell = devShells.default;
                };
            }
          # add network-docs & check-stylish to support
          # `nix build .\#hydraJobs.x86_64-linux.network-docs` and
          # `nix build .\#hydraJobs.x86_64-linux.check-stylis`.
          // { inherit network-docs check-stylish; };

        # Provide hydraJobs through legacyPackages to allow building without system prefix, e.g.
        # `nix build .\#network-mux:lib:network-mux`
        # `nix build .\#network-docs`
        legacyPackages = { inherit hydraJobs network-docs check-stylish; };
      in
      lib.recursiveUpdate flake rec {
        project = pkgs.ouroboros-network;
        inherit hydraJobs legacyPackages devShells;
        # formatter used by nix fmt
        formatter = pkgs.alejandra;
      }
    );
}
