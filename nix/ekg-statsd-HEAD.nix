{ mkDerivation, base, bytestring, ekg-core, fetchgit, network
, stdenv, text, time, unordered-containers
}:
mkDerivation {
  pname = "ekg-statsd";
  version = "0.2.4.0";
  src = fetchgit {
    url = "https://github.com/avieth/ekg-statsd";
    sha256 = "16wvyi9aifjvg9r9ppba3rg8ad2fnrjwaxdad5hg5r929ryhabn2";
    rev = "becbb8e7171d50763b0f80c5b2e9c966dcffb819";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring ekg-core network text time unordered-containers
  ];
  homepage = "https://github.com/tibbe/ekg-statsd";
  description = "Push metrics to statsd";
  license = stdenv.lib.licenses.bsd3;
}
