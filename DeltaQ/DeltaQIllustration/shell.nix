{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, Chart, Chart-cairo, containers
      , data-default, data-default-class, ieee754, lens, mtl, mwc-random
      , primitive, QuickCheck, random, statistics, stdenv
      }:
      mkDerivation {
        pname = "DeltaQIllustration";
        version = "0.1.5";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base Chart containers data-default ieee754 lens mtl mwc-random
          primitive statistics
        ];
        executableHaskellDepends = [
          base Chart Chart-cairo data-default-class lens mtl mwc-random
        ];
        testHaskellDepends = [ base Cabal QuickCheck random ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
