let
  # The tweag repository sources
  jupyterSrc = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "1176b9e8d173f2d2789705ad55c7b53a06155e0f";
    };

  # the jupyter build enviroment
  jupyter = (import jupyterSrc) {};

  nixpkgsPath = jupyterSrc + "/nix";

  # the pkgs in that environment
  pkgs = import nixpkgsPath {};

  # Override for packages included in the jupyter build.
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
           # version 1.9 is broken
           Chart = (hspkgs.callHackageDirect {pkg    = "Chart";
                                       ver    = "1.9.1";
                                       sha256 = "0aiq9r78yhma1xzhwl2xlzwqb2c59k6a4jjvdpninwm6lngac3s7";}) {};
           # see above
           Chart-cairo = (hspkgs.callHackageDirect {pkg    = "Chart-cairo";
                                       ver    = "1.9.1";
                                       sha256 = "0ambbkxbndjqxr0c4pysn87b3z0f8zpgvldj4qjbyazadna9k807";}) {};

           # 2019-07-08 - doesn't pass test suite: suppress test suite. Needed for pandoc.
           Diff = pkgs.haskell.lib.dontCheck (hspkgs.callHackage "Diff" "0.3.4" {});

           DeltaQIllustration = hspkgs.callCabal2nix "DeltaQIllustration" ./packages/DeltaQIllustration {};
           DeltaQIHaskell = hspkgs.callCabal2nix "DeltaQIHaskell" ./packages/DeltaQIHaskell {};
        });
      });

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  iHaskell = jupyter.kernels.iHaskellWith {
    haskellPackages = haskellPackages;
    name = "Chart";
    packages = p: with p; [
    # the pretty renders
    ihaskell-charts ihaskell-hatex ihaskell-diagrams ihaskell-graphviz ihaskell-aeson ihaskell-hvega ihaskell-magic
    Chart HaTeX diagrams graphviz # diagrams-graphviz - marked as broken
    formatting hvega pandoc
    DeltaQIllustration DeltaQIHaskell
   ];
  };

  # Creats a nix-shell
  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
      # note - may need to perform an initial action to create the
      # appropriate template for extentions.
      # directory = ./jupyterlab;
    };

  build = jupyterEnvironment;

  shell = pkgs.stdenv.mkDerivation {
    name = "my-env";
    buildInputs = [ build pkgs.graphviz haskellPackages.pandoc] ;
    };

in shell
