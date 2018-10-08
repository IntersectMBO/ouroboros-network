{ compiler ? "ghc843" }:
with builtins;
let
  rev = "61deecdc34fc609d0f805b434101f3c8ae3b807a";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  config = { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super:
                let QuickCheck = super.callPackage ./QuickCheck-2.12.4.nix {};
                in
                { free = super.callPackage ./free-5.1.nix {};
                  QuickCheck_2_12_4 = QuickCheck;
                  tasty-quickcheck = super.callPackage ./tasty-quickcheck-0.10.nix { inherit QuickCheck; };
                };
            };
            ghc843 = super.haskell.packages.ghc802.override {
              overrides = self: super:
                let QuickCheck = super.callPackage ./QuickCheck-2.12.4.nix {};
                in
                { free = super.callPackage ./free-5.1.nix {};
                  QuickCheck_2_12_4 = QuickCheck;
                  tasty-quickcheck = super.callPackage ./tasty-quickcheck-0.10.nix { inherit QuickCheck; };
                };
            };
            ghc861 = super.haskell.packages.ghc802.override {
              overrides = self: super:
                let QuickCheck = super.callPackage ./QuickCheck-2.12.4.nix {};
                in
                { free = super.callPackage ./free-5.1.nix {};
                  QuickCheck_2_12_4 = QuickCheck;
                  tasty-quickcheck = super.callPackage ./tasty-quickcheck-0.10.nix { inherit QuickCheck; };
                };
            };
          };
        };
      };
    };
in import (builtins.fetchTarball { inherit url; }) { inherit config; }
