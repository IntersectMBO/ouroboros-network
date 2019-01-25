{ mkDerivation, base, bytestring, stdenv, text }:
mkDerivation {
  pname = "polyparse";
  version = "1.12.1";
  sha256 = "dd8d34e05853ea0ab9b9fee1cbaa51ae33095f7c0c09ff539dcd6d771e0adaa5";
  libraryHaskellDepends = [ base bytestring text ];
  homepage = "http://code.haskell.org/~malcolm/polyparse/";
  description = "A variety of alternative parser combinator libraries";
  license = "LGPL";
}
