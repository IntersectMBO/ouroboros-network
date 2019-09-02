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
          (hsPkgs.small-steps)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.non-integer)
          (hsPkgs.cs-ledger)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-class)
          ];
        };
      tests = {
        "delegation-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.hedgehog)
            (hsPkgs.delegation)
            (hsPkgs.containers)
            (hsPkgs.multiset)
            (hsPkgs.text)
            (hsPkgs.microlens)
            (hsPkgs.cs-ledger)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "bf7038d160b1bba9c2611af63031fb1221508d7f";
      sha256 = "1ijrdjy7xq2p17lfsyn5fvssz2r8i17j14i361h28f7ki4znh52g";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/executable-spec; echo source root reset to \$sourceRoot";
    }