{
  description = "ouroboros-network";

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

  outputs = inputs: let
    # all platforms on which we build
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    # default compiler used on all systems, also provided within the shell
    defaultCompiler = "ghc982";

    # the compiler used for cross compilation
    # alternative compilers only used on Linux
    #
    # NOTE: cross compilation with `ghc-9.6.2` doesn't currently work
    # https://ci.iog.io/build/623082/nixlog/2
    crossGHCVersion = "ghc8107";

    # alternative compilers
    otherCompilers  = ["ghc810"];

    # from https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
    forAllProjectPackages = cfg: args@{ lib, ... }: {
      options.packages = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
          config = lib.mkIf config.package.isProject (cfg args);
        }));
      };
    };
  in
    {inherit (inputs);}
    // inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = [
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
          ];
        };
        inherit (nixpkgs) lib;
        haskellNix = import inputs.haskellNix { };

        # see flake `variants` below for alternative compilers
        inherit defaultCompiler;

        #
        # === CABAL PROJECT ===
        #

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, pkgs, ...}: {
          # pkgs - nixpkgs instatiated for cross compilation, so
          # stdenv.hostPlatform.isWindows will work as expected
          src = ./.;
          name = "ouroboros-network";
          compiler-nix-name = lib.mkDefault defaultCompiler;
          cabalProjectLocal = if pkgs.stdenv.hostPlatform.isWindows
                              then lib.readFile ./scripts/ci/cabal.project.local.Windows
                              else lib.readFile ./scripts/ci/cabal.project.local.Linux;

          #
          # CROSS COMPILATION
          # -----------------

          # we also want cross compilation to windows on linux (and only with default compiler).
          crossPlatforms =
            p: lib.optionals (nixpkgs.stdenv.hostPlatform.isLinux && config.compiler-nix-name == crossGHCVersion) [p.mingwW64];

          #
          # VARIANTS
          # --------

          # using different compilers
          flake.variants = (lib.genAttrs otherCompilers
                              (compiler-nix-name: { inherit compiler-nix-name; }));
          #
          # CHaP
          # ----

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };

          #
          # SHELL
          # -----

          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.10.3.0";
              ghcid = "0.8.9";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              stylish-haskell = "0.14.6.0";
              haskell-language-server = "2.7.0.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [ gh jq ];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          #
          # MODULES
          # -------

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            (forAllProjectPackages ({ ... }: {
              ghcOptions = [ "-Werror" ];
            }))
            ({pkgs, ...}: {
              # We impose limit heap size limit when running some of the tests
              # to discover space leaks Once #4698 and #4699 are done we can
              # further constrain the heap size.
              preCheck = lib.mkForce ''
              export GHCRTS=-M400M
              '';

              # pkgs are instantiated for the host platform
              packages.ouroboros-network-protocols.components.tests.cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag ];
              packages.ouroboros-network-protocols.components.tests.cddl.preCheck    = "export HOME=`pwd`";

              # don't run checks using Wine when cross compiling
              packages.quickcheck-monoids.components.tests.test.doCheck               = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ntp-client.components.tests.test.doCheck                       = !pkgs.stdenv.hostPlatform.isWindows;
              packages.network-mux.components.tests.test.doCheck                      = !pkgs.stdenv.hostPlatform.isWindows;
              packages.network-mux.components.tests.test.preCheck                     = "export GHCRTS=-M500M";
              packages.ouroboros-network-api.components.tests.test.doCheck            = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network-protocols.components.tests.test.doCheck      = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network-framework.components.tests.sim-tests.doCheck = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network-framework.components.tests.io-tests.doCheck  = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network-framework.components.tests.bench.doCheck     = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network.components.tests.sim-tests.doCheck           = !pkgs.stdenv.hostPlatform.isWindows;
              packages.ouroboros-network.components.tests.io-tests.doCheck            = !pkgs.stdenv.hostPlatform.isWindows;
              packages.cardano-client.components.tests.test.doCheck                   = !pkgs.stdenv.hostPlatform.isWindows;
            })
          ];
        });

        flake = cabalProject.flake {};

        #
        # HYDRA JOBS
        # ----------

        network-docs = nixpkgs.callPackage ./nix/network-docs.nix { };
        check-stylish = nixpkgs.callPackage ./nix/check-stylish.nix { };
      in
        lib.recursiveUpdate flake rec {
          project = cabalProject;
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
              {
                ciJobs =
                  flake.hydraJobs
                  // {
                    # This ensure hydra send a status for the required job (even if no change other than commit hash)
                    revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
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
          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling`
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject
            # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc927`
            // lib.mapAttrs (compiler-nix-name: _: let
              p = cabalProject.appendModule {inherit compiler-nix-name;};
            in
              p.shell // (profillingShell p))
            nixpkgs.haskell-nix.compiler;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
