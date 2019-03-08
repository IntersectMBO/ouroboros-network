{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "transformers-lift"; version = "0.2.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Vladislav Zavialov <vlad.z.4096@gmail.com>";
      author = "Vladislav Zavialov";
      homepage = "";
      url = "";
      synopsis = "Ad-hoc type classes for lifting";
      description = "This simple and lightweight library provides type classes\nfor lifting monad transformer operations.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.transformers)
          (hsPkgs.writer-cps-transformers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/int-index/transformers-lift";
      rev = "002555ea49ea7e76b401d09ab7367f3d5e582d70";
      sha256 = "0ryxm2i1w97f8gzlpvpcpn03aa16icb3hnn9qb29sny46wfsgg3n";
      });
    }