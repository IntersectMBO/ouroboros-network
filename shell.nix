{ compiler   ? "ghc862"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? localLib.pkgs
}:

let
  localPackages = import ./. { inherit compiler; };
  shell = localPackages.haskellPackages.shellFor {
    packages = p: (map (x: p.${x}) localLib.localPkgList);
    nativeBuildInputs = [ pkgs.cabal-install pkgs.haskellPackages.ghcid ];
  };

in shell
