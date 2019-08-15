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
      rev = "07b0caf22290dc1d5dc3a431ed75b823adaff6ff";
      sha256 = "081d6s91z0hlil60bzjvlfrwvmb6dw31ahk2hphbigw73yw3xlv3";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }