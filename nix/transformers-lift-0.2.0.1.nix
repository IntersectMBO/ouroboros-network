{ mkDerivation, base, stdenv, transformers, writer-cps-transformers
}:
mkDerivation {
  pname = "transformers-lift";
  version = "0.2.0.1";
  sha256 = "0bd8bf23fb29874daf9ff990bf25035e21208cfa292f9f18e8cfdb0b4b1ee09d";
  revision = "2";
  editedCabalFile = "16gpca2wfa7w2b5kzfvqsjjyd61pkv0wyi2mk5b34367p4chnsc5";
  libraryHaskellDepends = [
    base transformers writer-cps-transformers
  ];
  description = "Ad-hoc type classes for lifting";
  license = stdenv.lib.licenses.bsd3;
}
