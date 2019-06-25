let

  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "";
    }) {};

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };


  iHaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    packages = p: with p;
      let  dq1 = callPackage  ./packages/DeltaQIllustration {};
           dq2 = callPackage  ./packages/DeltaQIHaskell {DeltaQIllustration = dq1;};
      in [
#    ihaskell-charts
#     ihaskell-plot
    ihaskell-hatex
#     ihaskell-diagrams ihaskell-graphviz ihaskell-magic
#     ihaskell-aeson ihaskell-gnuplot ihaskell-widgets
#    formatting hvega
     dq1
     dq2

   ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
    };
in
  jupyterEnvironment.env
