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
      rev = "3a8b6c9401d62e50054f68e506d11cf76c3279e3";
      sha256 = "0dbzwy8awxrql4mqx6p8275kx1ihqn6zc3f0353kxz6bc94l903i";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }