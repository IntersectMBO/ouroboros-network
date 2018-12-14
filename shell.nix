{ compiler   ? "ghc862"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  default = import ./default.nix {inherit compiler haddock test;};
in
  {
    typed-transitions = if nixpkgs.lib.inNixShell
      then default.typed-transitions.env
      else default.typed-transitions;
    ouroboros-network = if nixpkgs.lib.inNixShell
      then default.ouroboros-network.env
      else default.ouroboros-network;
    ouroboros-consensus = if nixpkgs.lib.inNixShell
      then default.ouroboros-consensus.env
      else default.ouroboros-consensus;
  }

