{ mkDerivation, array, base, stdenv }:
mkDerivation {
  pname = "stm";
  version = "2.5.0.0";
  sha256 = "59e3685c66cbc54770d423f097ce50661005c99160be0f43a2b7fef7916494c6";
  libraryHaskellDepends = [ array base ];
  homepage = "https://wiki.haskell.org/Software_transactional_memory";
  description = "Software Transactional Memory";
  license = stdenv.lib.licenses.bsd3;
}
