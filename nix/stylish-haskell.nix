{ lib, runCommand, fetchFromGitHub, haskell-nix }:

let
  src = fetchFromGitHub {
    owner = "jaspervdj";
    repo = "stylish-haskell";
    rev = "3621bf3aa5312fef61220e1760d9988307209c6a";
    sha256 = "0rnzghsk3r28j3lcjb0s2mx1kdjrad0ynmsbx9b91158bq3vq09q";
  };

  pkgSet = haskell-nix.mkStackPkgSet {
    stack-pkgs = (haskell-nix.importAndFilterProject (haskell-nix.callStackToNix {
      inherit src;
    }));
    pkg-def-extras = [];
    modules = [];
  };

  packages = pkgSet.config.hsPkgs;
in
  packages.stylish-haskell.components.exes.stylish-haskell
