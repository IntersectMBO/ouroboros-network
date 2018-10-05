{ compiler ? "ghc843" }:
with builtins;
let
  fetchNixPkgs = import ./fetchNixPkgs.nix;
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
          };
        };
      };
    };
in import (fetchNixPkgs {}) { inherit config; }
