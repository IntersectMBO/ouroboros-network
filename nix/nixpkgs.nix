{}:
with builtins;
let
  rev = "cb95a3c1d1b6cd9da65650be269436cbe9e265fa";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  config = { packageOverrides = super:
      let self      = super.pkgs;
          lib       = super.haskell.lib;
          overrides = self: super:
            { psqueues = lib.dontCheck super.psqueues;
              aeson    = lib.dontCheck super.aeson;
            };
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc802 = super.haskell.packages.ghc802.override { inherit overrides; };
            ghc844 = super.haskell.packages.ghc844.override { inherit overrides; };
            ghc862 = super.haskell.packages.ghc862.override {
              overrides = super: self: overrides super self // {
                  hoopl_3_10_2_2 = self.callPackage ./hoopl-3.10.2.2.nix {};
              };
            };
          };
        };
      };
    };
in import (builtins.fetchTarball { inherit url; }) { inherit config; }
