{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-network-framework";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cborg)
          (hsPkgs.io-sim-classes)
          (hsPkgs.typed-protocols)
          (hsPkgs.serialise)
          (hsPkgs.typed-protocols-examples)
          ];
        };
      tests = {
        "ouroboros-network-framework-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.serialise)
            (hsPkgs.io-sim-classes)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-examples)
            (hsPkgs.ouroboros-network-framework)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../././ouroboros-network-framework;
    }