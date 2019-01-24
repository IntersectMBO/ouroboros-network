{ mkDerivation, base, bytestring, ekg-core, network, stdenv, text
, time, unordered-containers
}:
mkDerivation {
  pname = "ekg-statsd";
  version = "0.2.4.0";
  sha256 = "5e74bf63a1cd347c939d4eb7beb9181556b7bd033a60e5f6f4df0505e98a7adb";
  revision = "2";
  editedCabalFile = "1l0lh77qy4kbybkys1d4gg563fc593w27wpf4k1cg9j6ix6y604x";
  libraryHaskellDepends = [
    base bytestring ekg-core network text time unordered-containers
  ];
  homepage = "https://github.com/tibbe/ekg-statsd";
  description = "Push metrics to statsd";
  license = stdenv.lib.licenses.bsd3;
}
