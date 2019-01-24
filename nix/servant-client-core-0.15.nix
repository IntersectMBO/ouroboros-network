{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, deepseq, exceptions, free, generics-sop
, hspec, hspec-discover, http-media, http-types, network-uri
, QuickCheck, safe, servant, stdenv, template-haskell, text
, transformers
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.15";
  sha256 = "9b8e49e5e3cdda9216c393164e7c4b6d693bb159959dd52648f27f7adbca7960";
  libraryHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring containers
    deepseq exceptions free generics-sop http-media http-types
    network-uri safe servant template-haskell text transformers
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = stdenv.lib.licenses.bsd3;
}
