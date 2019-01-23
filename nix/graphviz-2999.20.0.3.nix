{ mkDerivation, base, bytestring, colour, containers, criterion
, deepseq, directory, dlist, fgl, fgl-arbitrary, filepath
, hspec, hspec-discover, mtl, polyparse, process, QuickCheck
, stdenv, temporary, text, wl-pprint-text
}:
mkDerivation {
  pname = "graphviz";
  version = "2999.20.0.3";
  sha256 = "efa0a27a914e4c51ebfc8b11a741f551e97713c22a02d0e60ddbd960f8376212";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring colour containers directory dlist fgl filepath mtl
    polyparse process temporary text wl-pprint-text
  ];
  testHaskellDepends = [
    base containers fgl fgl-arbitrary filepath hspec QuickCheck text
  ];
  # Causes infinite recursion since callPackage passes graphviz itself as this
  # dependency. So no tests...
  #testSystemDepends = [ graphviz ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base criterion deepseq text ];
  homepage = "http://projects.haskell.org/graphviz/";
  description = "Bindings to Graphviz for graph visualisation";
  license = stdenv.lib.licenses.bsd3;
}
