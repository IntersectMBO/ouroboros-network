{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "byron-proxy"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "alex@well-typed.com";
      author = "Alexander Vieth";
      homepage = "https://github.com/input-output-hk/ouroboros-network";
      url = "";
      synopsis = "Adapter for the Byron net";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cborg)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.free)
          (hsPkgs.io-sim)
          (hsPkgs.io-sim-classes)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.sqlite-simple)
          (hsPkgs.stm)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.transformers)
          ];
        };
      exes = {
        "byron-proxy" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.basic-tracer)
            (hsPkgs.byron-proxy)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.io-sim-classes)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.random)
            (hsPkgs.stm)
            (hsPkgs.text)
            (hsPkgs.time)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././byron-proxy; }