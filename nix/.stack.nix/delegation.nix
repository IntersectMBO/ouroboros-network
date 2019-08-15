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
          (hsPkgs.cardano-prelude)
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
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-prelude)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "3a8b6c9401d62e50054f68e506d11cf76c3279e3";
      sha256 = "0dbzwy8awxrql4mqx6p8275kx1ihqn6zc3f0353kxz6bc94l903i";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/executable-spec; echo source root reset to \$sourceRoot";
    }