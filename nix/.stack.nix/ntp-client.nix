{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.20";
      identifier = { name = "ntp-client"; version = "0.0.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.contra-tracer)
          (hsPkgs.network)
          (hsPkgs.stm)
          (hsPkgs.time)
          ];
        };
      exes = {
        "demo-ntp-client" = {
          depends = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.ntp-client)
            ];
          };
        };
      tests = {
        "test-ntp-client" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.ntp-client)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../././ntp-client; }