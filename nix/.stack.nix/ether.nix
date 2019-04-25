{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { disable-tup-instances = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "ether"; version = "9999"; };
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
      url = "https://github.com/input-output-hk/ether.git";
      rev = "a311878ae17ed5f201421bdcd5392a24b746ff0b";
      sha256 = "0xvbsap7brpyvac6v5k59vv7lfpdr9ridyz7nd8p27rwdlsn3ip9";
      });
    }