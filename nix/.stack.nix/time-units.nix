{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.6";
      identifier = { name = "time-units"; version = "1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Adam Wick <awick@uhusre.com>";
      author = "Adam Wick <awick@uhsure.com>";
      homepage = "http://github.com/acw/time-units";
      url = "";
      synopsis = "A basic library for defining units of time as types.";
      description = "In many cases, it is useful (either for error checking or documentation\nreasons) to define input and output types as having a particular unit of\ntime. In addition, by creating a type class defining type units, this\nlibrary should make it easier to separate the units of time the developer\nwants to think in versus the units of time the library author wants to\nthink in.";
      buildType = "Simple";
      };
    components = { "library" = { depends = [ (hsPkgs.base) ]; }; };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/acw/time-units";
      rev = "187c41725258b8881c527745b2c63cc5db880056";
      sha256 = "0qyhnzkb4cjqxa3k5clkx035hdplm5jij7dycxlhy1vr2ydgi4ib";
      });
    }