{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "small-steps"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.free)
          (hsPkgs.goblins)
          (hsPkgs.hedgehog)
          (hsPkgs.tasty-hunit)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.transformers)
          (hsPkgs.QuickCheck)
          (hsPkgs.cardano-crypto-class)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.free)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty-hunit)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.sequence)
            (hsPkgs.transformers)
            (hsPkgs.doctest)
            (hsPkgs.small-steps)
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover or (pkgs.buildPackages.doctest-discover))
            ];
          };
        "examples" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.hedgehog)
            (hsPkgs.mtl)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-expected-failure)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.tasty-hunit)
            (hsPkgs.Unique)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-binary)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "6d154d16de01edf2d3ff56303b7a34cb188ed217";
      sha256 = "010hgxnia3ryq74k9cf2yn39qnni1j27vg7xlzmdqr27p8ikkrw7";
      });
    postUnpack = "sourceRoot+=/byron/semantics/executable-spec; echo source root reset to \$sourceRoot";
    }