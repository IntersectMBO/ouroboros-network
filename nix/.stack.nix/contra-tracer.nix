{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "contra-tracer"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "A simple interface for logging, tracing or monitoring.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs.contravariant);
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "8614c66d9f13ad7bd68975700651a720a78d620e";
      sha256 = "1bz7kfbjqwf3kc80qqinwfqqmrmkmj5mf1cx9alvdmjdx65didiv";
      });
    postUnpack = "sourceRoot+=/contra-tracer; echo source root reset to \$sourceRoot";
    }