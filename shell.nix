{ withHoogle ? true
}:
let
  default = import ./default.nix {};
in
default.nix-tools._raw.shellFor {
  packages    = p: map (x: p."${x}") [
    "cabal-install"
    "io-sim"
    "io-sim-classes"
    "ouroboros-consensus"
    "ouroboros-network"
    "typed-transitions"
  ];
  withHoogle  = withHoogle;
}
