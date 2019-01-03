{ mkDerivation, nixpkgs, base, mtl, stdenv, stm }:
mkDerivation {
  pname = "io-sim-classes";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ "LICENSE" ".hs" "io-sim-classes.cabal" ];
  libraryHaskellDepends = [ base mtl stm ];
  description = "Type classes for concurrency with STM, ST and timing";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  enableSeparateDocOutput = false;
}
