{}:
with builtins;
let
  rev = "475d653afdbd8fe3e00ccfd22a30014b0df7aeaa";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  config = { packageOverrides = super:
      let self      = super.pkgs;
          lib       = super.haskell.lib;
          overrides = self: super:
            { psqueues = lib.dontCheck super.psqueues;
              aeson    = lib.dontCheck super.aeson;
              fingertree = super.callPackage ./fingertree-0.1.4.2.nix {};
              graphviz = lib.dontCheck super.graphviz;
              QuickCheck = super.QuickCheck_2_12_6_1;
              hspec = super.hspec_2_6_0;
              hspec-core = super.hspec-core_2_6_0;
              hspec-discover = super.hspec-discover_2_6_0;
              hspec-meta = super.hspec-meta_2_6_0;
              tasty-hspec = super.tasty-hspec_1_1_5_1;
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
