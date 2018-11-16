{ pkgs }:

with import ../../lib.nix;

with pkgs.haskell.lib;

let
  src = pkgs.lib.sourceFilesBySuffices ../.. [
    ".hs"
    "LICENSE"
    "ChangeLog.md"
    "ouroboros-network.cabal"
    "cabal.project"
  ];

in self: super: rec {
  ouroboros-network = self.callCabal2nix "ouroboros-network" src {};
}
