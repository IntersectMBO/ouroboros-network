{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "aeson-options"; version = "0.1.0"; };
      license = "MIT";
      copyright = "2018 Serokell";
      maintainer = "Serokell <hi@serokell.io>";
      author = "Serokell";
      homepage = "https://github.com/serokell/aeson-options";
      url = "";
      synopsis = "Options to derive FromJSON/ToJSON instances";
      description = "Options to derive FromJSON/ToJSON instances.";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) (hsPkgs.aeson) ]; };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/aeson-options";
      rev = "a25b839b6d2cf1b21fd53906eeaeb3e767cc369a";
      sha256 = "12czpbbj3h73cax3r3cmrrrnjm3wa1qwl3gr9zzw5hz9kag7blx0";
      });
    }