{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-slotting"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Key slotting types for cardano libraries";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-binary)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.serialise)
          (hsPkgs.transformers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "474622cfde663730e10e2e0d5de0ed06a867a844";
      sha256 = "1waqjyp6ycmn8lqrsnb8d14p43645mqknd5m0apb7814s3xd3f07";
      });
    postUnpack = "sourceRoot+=/slotting; echo source root reset to \$sourceRoot";
    }