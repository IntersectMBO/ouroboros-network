{ withHoogle ? true
}:
let
  default = import ./default.nix {};
in
default.nix-tools._raw.shellFor {
  packages    = ps: with ps; [
    io-sim
    io-sim-classes
    ouroboros-consensus
    ouroboros-network
    typed-transitions
  ];
  inherit withHoogle;
  buildInputs = with default.nix-tools._raw; [
    cabal-install.components.exes.cabal
  ];
}
