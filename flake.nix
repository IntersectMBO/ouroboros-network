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
          let
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

            # Sub-groups in flake.hydraJobs are cross-compilation targets
            # (e.g. x86_64-w64-mingw32) and compiler variants (e.g. ghc982).
            # They are distinguished from native job categories (packages,
            # checks, …) by having nested attrsets as values rather than
            # derivations.
            subGroups = lib.filterAttrs
              (_: v: lib.isAttrs v
                && !lib.isDerivation v
                && lib.any (x: lib.isAttrs x && !lib.isDerivation x)
                (lib.attrValues v))
              flake.hydraJobs;

            # For each sub-group expose an 'all' aggregate so that all jobs
            # in that group can be built with a single command, e.g.:
            #   nix build .\#hydraJobs.x86_64-linux.x86_64-w64-mingw32.all
            #   nix build .\#hydraJobs.x86_64-linux.ghc982.all
            subGroupAlls = lib.mapAttrs
              (name: jobs: {
                all = pkgs.releaseTools.aggregate {
                  name = "${name}-all";
                  meta.description = "All jobs for ${name} (no tests)";
                  constituents = lib.collect lib.isDerivation (builtins.removeAttrs jobs [ "checks" ]);
                };
              })
              subGroups;

            # Native-only jobs: ciJobs without the sub-groups, so that the
            # top-level 'all' is symmetric across systems (no cross-compiled
            # or variant jobs mixed in).
            nativeJobs = builtins.removeAttrs ciJobs (builtins.attrNames subGroups);
          in
          pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates { inherit ciJobs; }
          // subGroupAlls
          // {
            # An 'all' aggregate covering every native job on this system,
            # excluding tests, e.g.:
            #   nix build .\#hydraJobs.x86_64-linux.all
            #   nix build .\#hydraJobs.aarch64-darwin.all
            all = pkgs.releaseTools.aggregate {
              name = "all";
              meta.description = "All native jobs for ${system} (no tests)";
              constituents = lib.collect lib.isDerivation (builtins.removeAttrs nativeJobs [ "checks" ]);
            };
          };

        # Provide hydraJobs through legacyPackages to allow building without system prefix, e.g.
        # `nix build .\#network-mux:lib:network-mux`
        # `nix build .\#network-docs`
        legacyPackages = {
          inherit hydraJobs network-docs;
          format = format
            // {
            all = pkgs.releaseTools.aggregate {
              name = "network-format";
              meta.description = "Run all formatters";
              constituents = lib.collect lib.isDerivation format;
            };
          };
        };
      in
      lib.recursiveUpdate flake rec {
        project = pkgs.ouroboros-network;
        inherit hydraJobs legacyPackages devShells;
      }
    );
}
