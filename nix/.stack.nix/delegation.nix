{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "delegation"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Metheds Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Delegation Executable Model";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.hedgehog)
          (hsPkgs.QuickCheck)
          (hsPkgs.small-steps)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.mtl)
          (hsPkgs.non-integer)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.cs-ledger)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-class)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-slotting)
          ];
        };
      tests = {
        "delegation-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cborg)
            (hsPkgs.cryptonite)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.transformers)
            (hsPkgs.hedgehog)
            (hsPkgs.QuickCheck)
            (hsPkgs.delegation)
            (hsPkgs.containers)
            (hsPkgs.multiset)
            (hsPkgs.text)
            (hsPkgs.microlens)
            (hsPkgs.cs-ledger)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-slotting)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "07b0caf22290dc1d5dc3a431ed75b823adaff6ff";
      sha256 = "081d6s91z0hlil60bzjvlfrwvmb6dw31ahk2hphbigw73yw3xlv3";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/executable-spec; echo source root reset to \$sourceRoot";
    }