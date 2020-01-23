{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-slotting"; version = "0.1.0.0"; };
      license = "NONE";
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
<<<<<<< HEAD
      rev = "965b8b143632faea16680a195e6de57091382700";
      sha256 = "0x3cy5xa9mqdqnavs45assmmcrzw07qcwsv95capqwa6awz1plhh";
=======
      rev = "53d773907edea2fdcf65cebeb81ab5e47eeab805";
      sha256 = "1x3jacfggim9ynnw8rzaswhbvmhk13hid44k91a8137fv0jg6l14";
>>>>>>> 50731075... Update dependencies
      });
    postUnpack = "sourceRoot+=/slotting; echo source root reset to \$sourceRoot";
    }