{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "transformers-lift"; version = "9999"; };
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
      url = "https://github.com/input-output-hk/transformers-lift.git";
      rev = "c830bc7e7e41895e3c50488a92d34405b68b4ad8";
      sha256 = "022iiq9bg3kczv1awqzjllz6rd1dzcai9kivz26i8adnicp9b8xl";
      });
    }