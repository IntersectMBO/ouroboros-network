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

  # The ability to override pacakages for the jupter build.
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
           # dq1 = callCabal2nix "DeltaQIllustration"  ./packages/DeltaQIllustration {Chart=Chart;};
           # dq2 = callCabal2nix "DeltaQIHaskell"      ./packages/DeltaQIHaskell {DeltaQIllustration = dq1; Chart=Chart;};
          
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
    Chart HaTeX diagrams graphviz diagrams-graphviz
    formatting hvega
#    dq1
#    dq2
   ];
  };
 
  # Creats a nix-shell
  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
      # note - may need to perform an initial action to create the
      # appropriate template for extentions.
##      directory = ./jupyterlab;
    };

  build = jupyterEnvironment;

  shell = jupyterEnvironment.env;

# docker = mkDockerImage {inherit build};
# {inherit build shell; }
in {inherit build shell; }
     

