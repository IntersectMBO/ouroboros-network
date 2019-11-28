{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.6";
      identifier = { name = "Chart-cairo"; version = "1.9.1"; };
      license = "BSD-3-Clause";
      copyright = "Tim Docker, 2006-2014";
      maintainer = "Tim Docker <tim@dockerz.net>";
      author = "Tim Docker <tim@dockerz.net>";
      homepage = "https://github.com/timbod7/haskell-chart/wiki";
      url = "";
      synopsis = "Cairo backend for Charts.";
      description = "Cairo backend for Charts.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.old-locale)
          (hsPkgs.time)
          (hsPkgs.mtl)
          (hsPkgs.array)
          (hsPkgs.cairo)
          (hsPkgs.colour)
          (hsPkgs.data-default-class)
          (hsPkgs.operational)
          (hsPkgs.lens)
          (hsPkgs.Chart)
          ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../././peer-selection/haskell-chart/chart-cairo;
    }