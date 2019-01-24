{ mkDerivation, aeson, base, bytestring, ekg-core, ekg-json
, filepath, http-types, network, stdenv, text, time, transformers
, unordered-containers, wai, wai-app-static, warp
}:
mkDerivation {
  pname = "ekg-wai";
  version = "0.1.0.3";
  sha256 = "bfd35917b663da0c1354339dd30837eee6ddf0d42cf57442fd916a42c977a2e9";
  revision = "2";
  editedCabalFile = "17kca2wzlcv8nxyq096fv57jfklhz4ibnvf5nqqdszczb03j3dnn";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core ekg-json filepath http-types network
    text time transformers unordered-containers wai wai-app-static warp
  ];
  homepage = "https://github.com/tvh/ekg-wai";
  description = "Remote monitoring of processes";
  license = stdenv.lib.licenses.bsd3;
}
