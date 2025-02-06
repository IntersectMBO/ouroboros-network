# ouroboros-network overlay
inputs: final: prev:

let
  inherit (prev) lib;
  inherit (prev) pkgs;
  inherit (final) haskell-nix;
  buildSystem = pkgs.buildPlatform.system;

  # default compiler used on all systems, also provided within the shell
  defaultCompiler = "ghc982";

  # the compiler used for cross compilation
  # alternative compilers only used on Linux
  #
  # NOTE: cross compilation with `ghc-9.6.2` doesn't currently work
  # https://ci.iog.io/build/623082/nixlog/2
  crossGHCVersion = "ghc8107";

  # alternative compilers
  otherCompilers = [ "ghc810" ];

  # from https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
  forAllProjectPackages = cfg: args@{ lib, ... }: {
    options.packages = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
        config = lib.mkIf config.package.isProject (cfg args);
      }));
    };
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
      then lib.readFile ../scripts/ci/cabal.project.local.Windows
      else lib.readFile ../scripts/ci/cabal.project.local.Linux;

    #
    # CROSS COMPILATION
    # -----------------

    # we also want cross compilation to windows on linux (and only with default compiler).
    crossPlatforms =
      p: lib.optionals (pkgs.stdenv.hostPlatform.isLinux && config.compiler-nix-name == crossGHCVersion) [ p.mingwW64 ];

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
            then "export GHCRTS=-M400M"
            else "");
        doCheck = !pkgs.stdenv.hostPlatform.isWindows;

        # pkgs are instantiated for the host platform
        packages.ouroboros-network-protocols.components.tests.cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag ];
        packages.ouroboros-network-protocols.components.tests.cddl.preCheck = "export HOME=`pwd`";

        # don't run checks using Wine when cross compiling
        packages.network-mux.components.tests.test.preCheck =
          if buildSystem == "x86_64-linux" then "export GHCRTS=-M500M" else "";
      })
    ];
  });
in
{ inherit ouroboros-network; }
