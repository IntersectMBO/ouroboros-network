{ withHoogle ? true
, ... }@args:

let
  commonLib = import ./iohk-common.nix;
  pkgs      = commonLib.nix-tools.default-nix ./pkgs.nix args;
in pkgs // {
  shell = pkgs.nix-tools.shellFor {
    inherit withHoogle;
    packages = pkgs: with pkgs; [
      io-sim
      io-sim-classes
      ouroboros-consensus
      ouroboros-network
      typed-transitions
    ];
    buildInputs = with pkgs.nix-tools._raw; [
      cabal-install.components.exes.cabal
      commonLib.stack-hpc-coveralls
    ] ++ (with commonLib.pkgs; [
      git
      pkgconfig
      stack
      systemd
    ]);
  };
}
