{ mkDerivation, base, containers, directory, filepath
, generic-deriving, ghc-prim, mtl, stdenv, stm, tagged, tasty
, transformers, xml
}:
mkDerivation {
  pname = "tasty-ant-xml";
  version = "1.1.5";
  sha256 = "62ccee94bc5c3d7c6ed99037788262d8d971eeac487fe43b06760f969430a5df";
  libraryHaskellDepends = [
    base containers directory filepath generic-deriving ghc-prim mtl
    stm tagged tasty transformers xml
  ];
  homepage = "http://github.com/ocharles/tasty-ant-xml";
  description = "Render tasty output to XML for Jenkins";
  license = stdenv.lib.licenses.bsd3;
}
