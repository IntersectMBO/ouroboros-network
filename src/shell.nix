let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskell.packages.ghc843;
  # In case you want quantifie constraints. You'll need a bleeding-edge
  # nixpkgs.
  #haskellPackages = pkgs.haskell.packages.ghc861;
in
  { "typed-transitions" = haskellPackages.callPackage ./typed-transitions/default.nix { };
  }
