{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-db-test"; version = "3.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary instances for cardano-sl-db";
      description = "Cardano SL - arbitrary instances for cardano-sl-db";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f96f3fe04719bd3b0cedc1bbaf80fa6927e937f3";
      sha256 = "0wq3vqmb9r0g76zdx691gymvk7rdqdqxh5vgrfs8f5wpfwkhcxsi";
      });
    postUnpack = "sourceRoot+=/db/test; echo source root reset to \$sourceRoot";
    }