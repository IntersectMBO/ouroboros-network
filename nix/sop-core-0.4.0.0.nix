{ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "sop-core";
  version = "0.4.0.0";
  sha256 = "a381b0efb8e2dedb6627da6adb0a2b72421f87d43d9b53d68d5b2e866015911d";
  libraryHaskellDepends = [ base deepseq ];
  description = "True Sums of Products";
  license = stdenv.lib.licenses.bsd3;
}
