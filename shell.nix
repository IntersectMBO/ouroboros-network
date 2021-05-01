# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? { }
, sourcesOverride ? { }
, withHoogle ? false
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # Both stylish-haskell and Haskell-language-server are pulled in with niv
  # directly from their repositories at specific commits because the versions we
  # want are not yet in Hackage. Right now we are using stylish-haskell at
  # version 0.12.2.0 with some features that are not yet released in hackage,
  # and hls at version 1.1.0 which is also not released in hackage. Once one of
  # those arrives we can pull it in specifying a version on the tools attribute
  # set, something like:
  #
  # > tools = {
  # >   ...
  # >   haskell-language-server = "1.1.0";
  # >   stylish-haskell = "0.12.3.0";
  #
  # Note that the tools attribute comes from haskell-nix when defining the
  # shellFor function. At the same time, the niv depencency will not be needed
  # and it can be removed with `niv drop hls-released` or `niv drop
  # stylish-haskell` (or manually removing the relevant entries from
  # `sources.json`).
  stylish-haskell = import ./nix/stylish-haskell.nix { inherit pkgs; };
  hls = import ./nix/hls.nix { inherit pkgs; };

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  shell = ouroborosNetworkHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps)
      ++ [ ps.cardano-crypto-class ];

    # These programs will be available inside the nix-shell.
    buildInputs = [
      niv
      pkgconfig
      nixpkgs-fmt
      stylish-haskell
      hls.hls
      hls.hls-wrapper
      hls.implicit-hie
    ];

    tools = {
      ghcid = "0.8.7";
      cabal = "3.2.0.0";
      hasktags = "0.71.2";
      # https://hackage.haskell.org/package/graphmod
      graphmod = "1.4.4";
      # todo: add back the build tools which are actually necessary
      # ghcide = "0.2.0";
      # hlint = "...";
    };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = false;

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv
    ];
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

in
shell // { inherit devops; }
