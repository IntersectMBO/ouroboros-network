let

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/input-output-hk/nixpkgs/archive/55db2a95eade4a80fadf85ae7fce7fa293f87f50.tar.gz";
    sha256 = "0sll5drg4xrq4ap2lwpw9sr11003m0ampw0fs1xp7qgpi4dkp2rr";
  };

  # Grab the haskell.nix infrastructure.
  haskell-nix-src = builtins.fetchTarball {
    url = "https://github.com/avieth/haskell.nix/archive/c70dd72ae57aeca073fbeb6ad804f5c8032f0a34.tar.gz";
    sha256 = "18hlv1lsadd1ll2mb5bfsxl4j3xq71h4c5d20z36pcm28g2n3awj";
  };
  # This has keys config, overlays, suitable for passing to nixpkgs.
  haskell-nix = import haskell-nix-src;

  # We also need overlays for nixpkgs from jupyterWith.
  # The source of the IHaskell _cabal_ project. jupyterWith will not look for
  # a _default.nix_ here, it wants a _cabal_ file.
  ihaskell-src = builtins.fetchTarball {
    url = "https://github.com/avieth/IHaskell/archive/f0cccec63df6874ea0244adc4102caa4bff549fd.tar.gz";
    sha256 = "0wj88ys3qvjl7j3cj0krqy7im34naknpgc5ll8rzdzfdsyx550wl";
  };

  # Custom jupyterWith allows for setting the IHaskell and nixpkgs revisions.
  jupyterWith-src = builtins.fetchTarball {
    url = "https://github.com/avieth/jupyterWith/archive/4e5a9631bdcc4d832b6e71d9c45fda8f52d294e2.tar.gz";
    sha256 = "1jlhwi4wjyfh92gn40sglilbbwqcxsqvm5s5nfsav98j2303ihff";
  };

  # Include jupyterWith's python overlay in our nixpkgs ...
  jupyter-python-overlay  = import (jupyterWith-src + "/nix/python-overlay.nix");
  # ... along with the config and overlays from haskell.nix
  config = haskell-nix.config;
  overlays = haskell-nix.overlays ++ [ jupyter-python-overlay ];
  nixpkgs = import nixpkgs-src { inherit config; inherit overlays; };

  # This is where the haskell.nix stuff ends up.
  haskell = nixpkgs.haskell-nix;
  # Now the ouroboros-network package set can be made, using the haskell library
  # and nixpkgs. Seems weird that they are two _separate_ arguments when the
  # way haskell-nix was created seems to suggest that it is intimately linked
  # to the nixpkgs itself... but anyway... I am so confused.
  pkgs = import ../nix/pkgs.nix {
    haskell = haskell;
    pkgs = nixpkgs;
    extras = hackage: {
      # Must pick out a bunch of packages that are on hackage but not in the
      # LTS picked by ./nix/pkgs.nix
      packages = {
        # Chart-cairo needs a special revision to make it build with haskell.nix
        # (haskell.nix has a bug).
        "Chart-cairo" =
          let
            haskell-chart-src = builtins.fetchTarball {
              url = "https://github.com/avieth/haskell-chart/archive/054e6edd9d2e5c8d5a4aca52614537be4e195652.tar.gz";
              sha256 = "03jq78j4razdlp4ckfrrxfprcw4nxn1g4c5603jkm3nidadz7vsl";
            };
          in
            # haskell-chart-src is a string that is also kind of like a path.
            # You have to quote the relative path to add to it, but how would
            # you know that?
            import (haskell-chart-src + "/chart-cairo/Chart-cairo.nix");
        "cairo" = hackage.cairo."0.13.6.1".revisions.default;
        "gtk2hs-buildtools" = hackage.gtk2hs-buildtools."0.13.5.4".revisions.default;
        "ihaskell" = hackage.ihaskell."0.10.0.2".revisions.default;
        "ipython-kernel" = hackage.ipython-kernel."0.10.1.0".revisions.default;
        "ihaskell-charts" = hackage.ihaskell-charts."0.3.0.1".revisions.default;
        "ihaskell-graphviz" = hackage.ihaskell-graphviz."0.1.0.0".revisions.default;
        "graphviz" = hackage.graphviz."2999.20.0.3".revisions.default;
        "ghc-parser" = hackage.ghc-parser."0.2.1.0".revisions.default;
        "terminfo" = hackage.terminfo."0.4.1.2".revisions.default;
        "ghci" = hackage.ghci."8.6.5".revisions.default;
        "ghc" = hackage.ghc."8.6.5".revisions.default;
        "ghc-boot" = hackage.ghc-boot."8.6.5".revisions.default;
        "ghc-boot-th" = hackage.ghc-boot-th."8.6.5".revisions.default;
        "hpc" = hackage.hpc."0.6.0.3".revisions.default;
        "template-haskell" = hackage.template-haskell."2.15.0.0".revisions.default;
      };
    };
  };

  jupyterWith = import jupyterWith-src { inherit nixpkgs; };
  ihaskellKernel = jupyterWith.kernels.iHaskellWith {
    name = "haskell";
    haskellPackages = pkgs;
    ihaskellExe = pkgs.ihaskell.components.all;
    packages = ps: with ps; [
      Chart
      Chart-cairo
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
      ihaskell-charts
      ihaskell-graphviz

      ouroboros-network
    ];
  };
  ipythonKernel = jupyterWith.kernels.iPythonWith {
    name = "python";
    packages = ps: with ps; [numpy];
  };
  jupyterEnvironment = jupyterWith.jupyterlabWith {
    kernels = [ ipythonKernel ihaskellKernel ];
    extrapkgs = ps: with ps; [
      # graphviz is needed because ihaskell-graphviz shells out to the dot
      # program in order to produce an svg to show.
      graphviz
      ghc
    ];
  };

in
  jupyterEnvironment.env
