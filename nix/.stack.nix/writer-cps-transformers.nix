{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "writer-cps-transformers"; version = "0.5.5.0"; };
      license = "BSD-3-Clause";
      copyright = "2016 Daniel Mendler";
      maintainer = "mail@daniel-mendler.de";
      author = "Andy Gill, Ross Paterson, Daniel Mendler";
      homepage = "https://github.com/minad/writer-cps-transformers#readme";
      url = "";
      synopsis = "WriteT and RWST monad transformers";
      description = "The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. The corresponding MTL class instances are in the package writer-cps-mtl (<http://hackage.haskell.org/package/writer-cps-mtl>).";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) (hsPkgs.transformers) ]; };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/minad/writer-cps-transformers";
      rev = "971e29e0c77f5e47ad829f42ce465f23fa224a38";
      sha256 = "1gysq94705kxig6imnrpvcdi8jjshvr85achjrqaf6aimx8ww2s6";
      });
    }