{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { disable-tup-instances = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "ether"; version = "0.5.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Vladislav Zavialov <vlad.z.4096@gmail.com>";
      author = "Vladislav Zavialov";
      homepage = "https://int-index.github.io/ether/";
      url = "";
      synopsis = "Monad transformers and classes";
      description = "Ether is a Haskell library that extends @mtl@ and @transformers@ with\ntagged monad transformers and classes in a compatible way.\nIntroduction <https://int-index.github.io/ether/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.transformers)
          (hsPkgs.transformers-lift)
          (hsPkgs.mtl)
          (hsPkgs.mmorph)
          (hsPkgs.monad-control)
          (hsPkgs.transformers-base)
          (hsPkgs.writer-cps-mtl)
          (hsPkgs.exceptions)
          (hsPkgs.template-haskell)
          (hsPkgs.tagged)
          (hsPkgs.reflection)
          ];
        };
      tests = {
        "regression" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.transformers)
            (hsPkgs.mtl)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.QuickCheck)
            (hsPkgs.ghc-prim)
            (hsPkgs.lens)
            (hsPkgs.ether)
            ];
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.mtl)
            (hsPkgs.transformers)
            (hsPkgs.criterion)
            (hsPkgs.deepseq)
            (hsPkgs.lens)
            (hsPkgs.ether)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/int-index/ether";
      rev = "84c1d560da241c8111d1a3c98d9a896f0c62087b";
      sha256 = "0h2md24q9dhxh5r79dy7shry7yxgwf45735fqwlx3j2z0znq9vxs";
      });
    }