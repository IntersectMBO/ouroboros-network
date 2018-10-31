{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  default = import ./default.nix {inherit compiler haddock test;};
in
  {
    ouroboros-network = if nixpkgs.lib.inNixShell
      then default.ouroboros-network.env
      else default.ouroboros-network;
    typed-transitions = if nixpkgs.lib.inNixShell
      then default.typed-transitions.env
      else default.typed-transitions;
  }

