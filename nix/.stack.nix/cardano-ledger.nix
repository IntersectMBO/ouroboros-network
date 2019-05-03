{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-ledger"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano";
      description = "The blockchain layer of Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.base58-bytestring)
          (hsPkgs.base64-bytestring-type)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.containers)
          (hsPkgs.concurrency)
          (hsPkgs.cryptonite)
          (hsPkgs.Cabal)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.resourcet)
          (hsPkgs.streaming)
          (hsPkgs.streaming-binary)
          (hsPkgs.streaming-bytestring)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-binary-test)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-crypto-test)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-mainnet-mirror)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-prelude-test)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.cs-blockchain)
            (hsPkgs.cs-ledger)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.resourcet)
            (hsPkgs.small-steps)
            (hsPkgs.streaming)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.vector)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "12838595066398cbe49f6ea1f0c4f4c03882e530";
      sha256 = "13904x9bg9yrykqy73aglsacck5h761359wca0bx234g39fi1fy0";
      });
    }