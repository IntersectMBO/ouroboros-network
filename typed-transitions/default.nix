{ mkDerivation, stdenv, base, async, free, network, stm, nixpkgs }:
mkDerivation {
  pname = "typed-transitions";
  version = "0.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "typed-transitions.cabal" "cabal.project" ];
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base async free network stm ];
  license = stdenv.lib.licenses.bsd3;
}
