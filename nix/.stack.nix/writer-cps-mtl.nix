{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "writer-cps-mtl"; version = "0.1.1.6"; };
      license = "BSD-3-Clause";
      copyright = "2016 Daniel Mendler";
      maintainer = "mail@daniel-mendler.de";
      author = "Andy Gill, Edward Kmett, Daniel Mendler";
      homepage = "https://github.com/minad/writer-cps-mtl#readme";
      url = "";
      synopsis = "MonadWriter orphan instances for writer-cps-transformers";
      description = "The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. See also (<http://hackage.haskell.org/package/writer-cps-transformers>).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.mtl)
          (hsPkgs.transformers)
          (hsPkgs.writer-cps-transformers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/minad/writer-cps-mtl";
      rev = "efee5993637c4d844938ae2b2668079f4df0519f";
      sha256 = "1n5s7la4908f6kix9ipzywaqlcz0qfcwvx42zlxqjggh2hnjq0qv";
      });
    }) // { cabal-generator = "hpack"; }