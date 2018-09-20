let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskell.packages.ghc841;
  # In case you want quantifie constraints. You'll need a bleeding-edge
  # nixpkgs.
  #haskellPackages861 = pkgs.haskell.packages.ghc861;
in
  { "typed-transitions" = haskellPackages.callPackage ./typed-transitions.nix { };
  }
