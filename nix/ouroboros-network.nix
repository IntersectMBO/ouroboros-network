# ouroboros-network overlay
inputs: final: prev:

let
  inherit (prev) lib;
  inherit (prev) pkgs;
  inherit (final) haskell-nix;
  buildSystem = pkgs.buildPlatform.system;
  onLinux = buildSystem == "x86_64-linux";

  # default compiler used on all systems, also provided within the shell
  defaultCompiler = "ghc966";

  # the compiler used for cross compilation
  # alternative compilers only used on Linux
  #
  # NOTE: cross compilation with `ghc-9.6.2` doesn't currently work
  # https://ci.iog.io/build/623082/nixlog/2
  crossGHCVersion = "ghc966";

  # alternative compilers
  otherCompilers =
    if onLinux then [ "ghc982" ] else [ ];

  # from https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
  forAllProjectPackages = cfg: args@{ config, lib, ... }: {
    options.packages =
      lib.genAttrs config.package-keys (_:
        lib.mkOption {
          type = lib.types.submodule ({ config, ... }:
            {
              config = lib.mkIf config.package.isProject (cfg args);
            }
          );
        }
      );
  };

  # We use cabalProject' to ensure we don't build the plan for
  # all systems.
  ouroboros-network = haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    # pkgs - nixpkgs instatiated for cross compilation, so
    # stdenv.hostPlatform.isWindows will work as expected
    src = ./..;
    name = "ouroboros-network";
    compiler-nix-name = lib.mkDefault defaultCompiler;
    cabalProjectLocal =
      if pkgs.stdenv.hostPlatform.isWindows
      then lib.readFile ../scripts/ci/cabal.project.local.Nix.Windows
      else lib.readFile ../scripts/ci/cabal.project.local.Linux;

    #
    # CROSS COMPILATION
    # -----------------

    # we also want cross compilation to windows on linux (and only with default compiler).
    crossPlatforms =
      p: lib.optionals (pkgs.stdenv.hostPlatform.isLinux && config.compiler-nix-name == crossGHCVersion) [ p.ucrt64 p.musl64 ];

    #
    # VARIANTS
    # --------

    # using different compilers
    flake.variants =
      # otherCompilers
      (lib.genAttrs otherCompilers
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
    # MODULES
    # -------

    # package customizations as needed. Where cabal.project is not
    # specific enough, or doesn't allow setting these.
    modules = [
      (forAllProjectPackages ({ ... }: {
        ghcOptions = [ "-Werror" "-fno-ignore-asserts" ];
      }))
      ({ pkgs, ... }: {
        # We impose limit heap size limit when running some of the tests
        # to discover space leaks Once #4698 and #4699 are done we can
        # further constrain the heap size.
        preCheck =
          lib.mkForce
            (if buildSystem == "x86_64-linux"
            then "export GHCRTS=-M300M"
            else "");
        doCheck = !pkgs.stdenv.hostPlatform.isWindows;

        # pkgs are instantiated for the host platform
        packages.cardano-diffusion.components.tests.protocols-cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag pkgs.cddlc ];
        packages.cardano-diffusion.components.tests.protocols-cddl.preCheck = "export HOME=`pwd`";
        # note: protocols-cddl is disabled on Windows in ./scripts/ci/cabal.project.local.Windows

        packages.dmq-node.components.tests.dmq-cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag pkgs.cddlc ];
        packages.dmq-node.components.tests.dmq-cddl.preCheck = "export HOME=`pwd`";

        # pkgs are disabled since we don't have enough CPU bandwidth on MacOS machines
        packages.ouroboros-network.components.tests.framework-sim-tests.doCheck = onLinux;
        packages.ouroboros-network.components.tests.ouroboros-network-sim-tests.doCheck = onLinux;

        packages.dmq-node.components.tests.dmq-tests.preCheck =
          if buildSystem == "x86_64-linux" then "export GHCRTS=-M2500M" else "";
        packages.network-mux.components.tests.test.preCheck =
          if buildSystem == "x86_64-linux" then "export GHCRTS=-M800M" else "";
        packages.ouroboros-network.components.tests.protocols-test.preCheck =
          if buildSystem == "x86_64-linux" then "export GHCRTS=-M800M" else "";
        packages.cardano-diffusion.components.tests.cardano-diffusion-sim-tests.preCheck =
          if buildSystem == "x86_64-linux" then "export GHCRTS=-M7000M" else "";
      })
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        packages.basement.configureFlags = [ "--hsc2hs-options=--cflag=-Wno-int-conversion" ];
      })
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
        # rubby fails to build with musl, hence we disable cddl tests
        packages.dmq-node.components.tests.dmq-cddl.build-tools = lib.mkForce [ ];
        packages.dmq-node.components.tests.dmq-cddl.doCheck = lib.mkForce false;
        packages.cardano-diffusion.components.tests.protocols-cddl.build-tools = lib.mkForce [ ];
        packages.cardano-diffusion.components.tests.protocols-cddl.doCheck = lib.mkForce false;
        packages.dmq-node.ghcOptions = with pkgs; [
          "-L${lib.getLib static-gmp}/lib"
          "-L${lib.getLib static-libsodium-vrf}/lib"
          "-L${lib.getLib static-secp256k1}/lib"
          "-L${lib.getLib static-libblst}/lib"
        ];
      })
    ];
  });
in
{ inherit ouroboros-network; }
