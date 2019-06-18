{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-infra-test"; version = "3.0.2"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - generators for cardano-sl-infra";
      description = "This package contains generators for the infrastructural data types used in Cardano SL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary-test)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.containers)
          (hsPkgs.dns)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.iproute)
          (hsPkgs.kademlia)
          (hsPkgs.universum)
          (hsPkgs.yaml)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "16f5095cbf5d1128e379b44c10ff4114253cefb9";
      sha256 = "1qhrf2mmnmmjvl325ha8vghc6mnm72q9vab0x0df70sxcknhv5ay";
      });
    postUnpack = "sourceRoot+=/infra/test; echo source root reset to \$sourceRoot";
    }