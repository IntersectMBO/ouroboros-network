{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "peer-selection"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "aovieth@gmail.com";
      author = "Neil Davies, Alexander Vieth";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.Chart)
          (hsPkgs.Chart-cairo)
          (hsPkgs.cairo)
          (hsPkgs.colour)
          (hsPkgs.containers)
          (hsPkgs.data-default-class)
          (hsPkgs.deepseq)
          (hsPkgs.fingertree)
          (hsPkgs.graphviz)
          (hsPkgs.mwc-random)
          (hsPkgs.primitive)
          (hsPkgs.random)
          (hsPkgs.statistics)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.vector)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../././peer-selection; }