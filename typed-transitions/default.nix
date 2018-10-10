{ mkDerivation, stdenv, base, async, network }:
mkDerivation {
  pname = "typed-transitions";
  version = "0.0.0";
  src = "./.";
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base async network ];
  license = stdenv.lib.licenses.bsd3;
}
