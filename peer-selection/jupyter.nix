let

  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6695d3f08f213976b781fa0a3b12f665b1857414.tar.gz";
    sha256 = "1mr0mrs59z0swx9f95j0bcn4fyhwh9h06vwim42nx7ya3xa1lihi";
  };
  nixpkgs = import nixpkgsSrc;

  # The source of the IHaskell _cabal_ project. jupyterWith will not look for
  # a _default.nix_ here, it wants a _cabal_ file.
  ihaskellSrc = builtins.fetchTarball {
    url = "https://github.com/avieth/IHaskell/archive/f0cccec63df6874ea0244adc4102caa4bff549fd.tar.gz";
    sha256 = "0wj88ys3qvjl7j3cj0krqy7im34naknpgc5ll8rzdzfdsyx550wl";
  };

  # Custom jupyterWith allows for setting the IHaskell and nixpkgs revisions.
  jupyterWithSrc = builtins.fetchTarball {
    url = "https://github.com/avieth/jupyterWith/archive/f9d739aa3da4016b5042acff7b296b8409684274.tar.gz";
    sha256 = "1fx4qfg7l6g7jz1fqx755d629r7i3zlcbvp7434axaxddxjpa3i6";
  };
  jupyterWith = import jupyterWithSrc { inherit nixpkgs; ihaskell = ihaskellSrc; };

  # jupyterWith creates the nixpkgs (applies overlays).
  pkgs = jupyterWith.pkgs;

  dontCheck = pkgs.haskell.lib.dontCheck;
  dontHaddock = pkgs.haskell.lib.dontHaddock;

  # Some overrides to make it build.
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides (self: hspkgs: {
      cabal2nix = hspkgs.cabal2nix.overrideScope (self: super: { Cabal = self.Cabal_2_4_1_0; });
      system-fileio = dontCheck hspkgs.system-fileio;
      Diff = dontCheck hspkgs.Diff;
      # Haddock syntax error in this package...
      ghc-lib-parser_8_8_1 = dontHaddock hspkgs.ghc-lib-parser_8_8_1;
    });
  });

  ihaskellKernel = jupyterWith.kernels.iHaskellWith {
    haskellPackages = haskellPackages;
    name = "haskell";
    packages = p: with p; [
      Chart
      Chart-cairo
      ihaskell-charts
      ihaskell-graphviz
      cairo
      colour
      containers
      data-default-class
      deepseq
      diagrams
      fingertree
      graphviz
      random
      statistics
      time
      vector
    ];
  };

  ipythonKernel = jupyterWith.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  # BUG: if you give no kernels ([]) it fails...
  jupyterEnvironment = jupyterWith.jupyterlabWith {
    kernels = [ ipythonKernel ihaskellKernel ];
    # Need the dot executable in order to use ihaskell-graphviz
    extrapkgs = pkgs: with pkgs; [
      graphviz
      ghc
    ];
  };
in
  jupyterEnvironment.env
