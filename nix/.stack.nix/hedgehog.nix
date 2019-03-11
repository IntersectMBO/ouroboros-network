{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "hedgehog"; version = "0.6.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "https://hedgehog.qa";
      url = "";
      synopsis = "Hedgehog will eat all your bugs.";
      description = "Hedgehog is a modern property-based testing system, in the spirit of\nQuickCheck. Hedgehog uses integrated shrinking, so shrinks obey the\ninvariants of generated values by construction.\n\nTo get started quickly, see the examples:\n<https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.ansi-terminal)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.concurrent-output)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.lifted-async)
          (hsPkgs.mmorph)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.pretty-show)
          (hsPkgs.primitive)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.semigroups)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.th-lift)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.transformers-base)
          (hsPkgs.wl-pprint-annotated)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.hedgehog)
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.pretty-show)
            (hsPkgs.semigroups)
            (hsPkgs.text)
            (hsPkgs.transformers)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/haskell-hedgehog";
      rev = "e63fb354bfbad07f3befdd43b382c655944218be";
      sha256 = "1p2yzlaiqj34pkk26cgg0dkvkm1zwjpgx1zk95md5xj0cijb34gj";
      });
    postUnpack = "sourceRoot+=/hedgehog; echo source root reset to \$sourceRoot";
    }