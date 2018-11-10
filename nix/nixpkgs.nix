{ compiler ? "ghc844" }:
with builtins;
let
  rev = "722fcbbb80b2142583e9266efe77992f8e32ac4c";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  config = { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
          overrides = self: super:
            { free = super.free_5_1;
              stm = super.stm_2_5_0_0;
              QuickCheck = super.QuickCheck_2_12_6_1;
              psqueues = lib.dontCheck(super.psqueues);
              # aeson from git@github.com:coot/aeson (QuickCheck-2.12)
              aeson = super.callPackage ./aeson.nix {};
              # aeson requirement
              hspec = super.hspec_2_5_8;
              hspec-core = super.hspec-core_2_5_8;
              hspec-discover = super.hspec-discover_2_5_8;
              hspec-meta = super.hspec-meta_2_5_6;
            };
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc802 = super.haskell.packages.ghc802.override { inherit overrides; };
            ghc844 = super.haskell.packages.ghc844.override { inherit overrides; };
          };
        };
      };
    };
in import (builtins.fetchTarball { inherit url; }) { inherit config; }
