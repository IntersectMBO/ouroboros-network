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
                  cborg = super.callPackage ./cborg-0.2.1.0.nix {};
                  # aeson from git@github.com:coot/aeson (QuickCheck-2.12)
                  aeson = super.callPackage ./aeson.nix {};
                  # aeson requirement
                  quickcheck-instances = super.callPackage ./quickcheck-instances-0.3.19.nix { inherit QuickCheck; };
                  # aeson requirement
                  time-locale-compat = super.time-locale-compat_0_1_1_5;
                };
            };
            ghc843 = super.haskell.packages.ghc843.override {
              overrides = self: super:
                let QuickCheck = super.callPackage ./QuickCheck-2.12.4.nix {};
                in
                { free = super.callPackage ./free-5.1.nix {};
                  QuickCheck_2_12_4 = QuickCheck;
                  tasty-quickcheck = super.callPackage ./tasty-quickcheck-0.10.nix { inherit QuickCheck; };
                  cborg = super.callPackage ./cborg-0.2.1.0.nix {};
                  # aeson from git@github.com:coot/aeson (QuickCheck-2.12)
                  aeson = super.callPackage ./aeson.nix {};
                  # aeson requirement
                  quickcheck-instances = super.callPackage ./quickcheck-instances-0.3.19.nix { inherit QuickCheck; };
                  # aeson requirement
                  time-locale-compat = super.time-locale-compat_0_1_1_5;
                };
            };
            ghc861 = super.haskell.packages.ghc861.override {
              overrides = self: super:
                let QuickCheck = super.callPackage ./QuickCheck-2.12.4.nix {};
                in
                { free = super.callPackage ./free-5.1.nix {};
                  QuickCheck_2_12_4 = QuickCheck;
                  tasty-quickcheck = super.callPackage ./tasty-quickcheck-0.10.nix { inherit QuickCheck; };
                  cborg = super.callPackage ./cborg-0.2.1.0.nix {};
                  # aeson from git@github.com:coot/aeson (QuickCheck-2.12)
                  aeson = super.callPackage ./aeson.nix {};
                  # aeson requirement
                  quickcheck-instances = super.callPackage ./quickcheck-instances-0.3.19.nix { inherit QuickCheck; };
                  # aeson requirement
                  time-locale-compat = super.time-locale-compat_0_1_1_5;
                };
            };
          };
        };
      };
    };
in import (builtins.fetchTarball { inherit url; }) { inherit config; }
