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
            (import ./nix/ouroboros-network.nix inputs)
          ];
        };
        inherit (pkgs) lib;

        flake = pkgs.ouroboros-network.flake {};
        network-docs = pkgs.callPackage ./nix/network-docs.nix { };
        check-stylish = pkgs.callPackage ./nix/check-stylish.nix { };
        hydraJobs =
          pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = pkgs.writeText "revision" (inputs.self.rev or "dirty");
                  inherit network-docs check-stylish;
                };
            }
          # add network-docs & check-stylish to support
          # `nix build .\#hydraJobs.x86_64-linux.network-docs` and
          # `nix build .\#hydraJobs.x86_64-linux.check-stylis`.
          // { inherit network-docs check-stylish; };
        # also provide hydraJobs through legacyPackages to allow building without system prefix, e.g.
        # `nix build .\#network-mux:lib:network-mux`
        # `nix build .\#network-docs`
        legacyPackages = { inherit hydraJobs network-docs check-stylish; };
      in
      lib.recursiveUpdate flake rec {
        project = pkgs.ouroboros-network;
        inherit hydraJobs;
        inherit legacyPackages;
        devShells = let
          profillingShell = p: {
            # `nix develop .#profiling`
            profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
          };
        in
          profillingShell pkgs.ouroboros-network
          # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc927`
          // lib.mapAttrs (compiler-nix-name: _: let
            p = pkgs.ouroboros-network.appendModule {inherit compiler-nix-name;};
          in
            p.shell // (profillingShell p))
          pkgs.haskell-nix.compiler;
        # formatter used by nix fmt
        formatter = pkgs.alejandra;
      }
    );
}
