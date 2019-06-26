{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, DeltaQIllustration, HaTeX, ihaskell
      , ihaskell-hatex, stdenv
      }:
      mkDerivation {
        pname = "DeltaQIHaskell";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base DeltaQIllustration HaTeX ihaskell ihaskell-hatex
        ];
        description = "Interface module to isolate DeltaQ libraries from IHaskell/Jupyter requirements";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
