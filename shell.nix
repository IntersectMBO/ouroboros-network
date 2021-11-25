# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? { }, sourcesOverride ? { }, withHoogle ? false
, pkgs ? import ./nix { inherit config sourcesOverride; } }:
with pkgs;
let
  # Haskell-language-server is pulled in with niv directly from its repositories
  # at specific commit because the version we want is not yet in our hackage
  # index (dictated by haskell.nix's version). Once it arrives we can pull it in
  # specifying a version on the tools attribute set, something like:
  #
  # > tools = {
  # >   ...
  # >   haskell-language-server = "1.5.1.0";
  #
  # Note that the tools attribute comes from haskell-nix when defining the
  # shellFor function. At the same time, the niv depencency will not be needed
  # and it can be removed with `niv drop hls-released` (or manually removing the
  # relevant entries from `sources.json`).
  hls = import ./nix/hls.nix { inherit pkgs; };

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = ouroborosNetworkHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    packages = ps:
      lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = [
      cabalWrapped
      entr
      niv
      pkgconfig
      nixpkgs-fmt
      hls.hls
      hls.hls-wrapper
      hls.implicit-hie
    ];

    tools = {
      # IDE tools
      stylish-haskell = "0.13.0.0";
      ghcid = "0.8.7";
      hasktags = "0.71.2";
      # Draw graph of module dependencies
      graphmod = "1.4.4";
      # Profiling tools
      profiteur = "0.4.6.0";
      eventlog2html = "0.9.2";
      hp2pretty = "0.10";
    };

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [ niv ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in shell // { inherit devops; }
