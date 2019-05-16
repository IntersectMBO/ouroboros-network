{ withHoogle ? true
}:
let
  default = import ./default.nix {};
in
default.nix-tools._raw.shellFor {
  packages    = p: map (x: p."${x}") [
    "io-sim"
    "io-sim-classes"
    "ouroboros-consensus"
    "ouroboros-network"
    "typed-transitions"
  ];
  withHoogle  = withHoogle;
  buildInputs = with default.nix-tools._raw; [
    cabal-install.components.exes.cabal
  ];
}
