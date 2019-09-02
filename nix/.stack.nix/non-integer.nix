{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "non-integer"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) ]; };
      exes = {
        "nonInt" = { depends = [ (hsPkgs.base) (hsPkgs.non-integer) ]; };
        };
      tests = {
        "non-integer-test" = {
          depends = (pkgs.lib).optionals (!flags.development) [
            (hsPkgs.base)
            (hsPkgs.non-integer)
            (hsPkgs.QuickCheck)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "bf7038d160b1bba9c2611af63031fb1221508d7f";
      sha256 = "1ijrdjy7xq2p17lfsyn5fvssz2r8i17j14i361h28f7ki4znh52g";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }