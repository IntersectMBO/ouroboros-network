{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, data-default, ieee754, lens
      , mtl, mwc-random, primitive, statistics, stdenv
      }:
      mkDerivation {
        pname = "DeltaQIllustration";
        version = "0.1.5";
        src = ./.;
        libraryHaskellDepends = [
          base containers data-default ieee754 lens mtl mwc-random primitive
          statistics
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
