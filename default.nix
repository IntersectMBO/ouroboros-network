# Builds using system <nixpkgs> and ghc863 with the required package curation.
let
  nixpkgs = import <nixpkgs> {};
  compiler = import ./nix/ghc.nix { inherit nixpkgs; };
in
  import ./pkgs.nix { inherit nixpkgs; inherit compiler; }
