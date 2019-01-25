{ mkDerivation, aeson, base, bytestring, ekg-core, ekg-json
, filepath, network, snap-core, snap-server, stdenv, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "ekg";
  version = "0.4.0.15";
  sha256 = "482ae3be495cfe4f03332ad1c79ce8b5ad4f9c8eec824980c664808ae32c6dcc";
  revision = "5";
  editedCabalFile = "0jwzwqr4giinq6wvl46399454nm9vc5g6mc2k2mx4wjdcl07qbgm";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core ekg-json filepath network snap-core
    snap-server text time transformers unordered-containers
  ];
  homepage = "https://github.com/tibbe/ekg";
  description = "Remote monitoring of processes";
  license = stdenv.lib.licenses.bsd3;
}
