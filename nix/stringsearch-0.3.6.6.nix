{ mkDerivation, array, base, bytestring, containers, stdenv }:
mkDerivation {
  pname = "stringsearch";
  version = "0.3.6.6";
  sha256 = "295f1971920bc52263d8275d7054ad223a7e1aefe75533f9887735c9644ffe4a";
  revision = "1";
  editedCabalFile = "0z5pz5dccapz9k39r2zmf056m0x2m2lj3jahhnw3mfxlmps07378";
  libraryHaskellDepends = [ array base bytestring containers ];
  homepage = "https://bitbucket.org/dafis/stringsearch";
  description = "Fast searching, splitting and replacing of ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
