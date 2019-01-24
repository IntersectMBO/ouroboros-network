{ mkDerivation, attoparsec, base, base-compat, stdenv, text, time
}:
mkDerivation {
  pname = "attoparsec-iso8601";
  version = "1.0.1.0";
  sha256 = "499ffbd2d39e79cc4fda5ad0129dbf94fdb72a84aa932dfe2a5f5c5c02074142";
  libraryHaskellDepends = [ attoparsec base base-compat text time ];
  homepage = "https://github.com/bos/aeson";
  description = "Parsing of ISO 8601 dates, originally from aeson";
  license = stdenv.lib.licenses.bsd3;
}
