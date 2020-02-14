# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? false
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = ouroborosNetworkHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: with ps; [
       ouroboros-network
       ouroboros-consensus
       typed-protocols
       typed-protocols-examples
       Win32-network
       network-mux
       ouroboros-network-framework
       ouroboros-network
       ouroboros-network-testing
       ouroboros-consensus
       io-sim
       io-sim-classes
       ntp-client
    ];

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      ghcid
      hlint
      weeder
      nix
      niv
      pkgconfig
      sqlite-interactive
      git
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

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
