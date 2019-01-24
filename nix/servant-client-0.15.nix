{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, entropy, exceptions, generics-sop, hspec, hspec-discover
, http-api-data, http-client, http-media, http-types, HUnit
, kan-extensions, markdown-unlit, monad-control, mtl, network
, QuickCheck, semigroupoids, servant, servant-client-core
, servant-server, stdenv, stm, tdigest, text, time, transformers
, transformers-base, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.15";
  sha256 = "2a6c731a479f68ea8f7fe3e124b8b87d14ca9c385ed0751a70461a3c59540a25";
  libraryHaskellDepends = [
    base base-compat bytestring containers deepseq exceptions
    http-client http-media http-types kan-extensions monad-control mtl
    semigroupoids servant servant-client-core stm text time
    transformers transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring entropy generics-sop hspec
    http-api-data http-client http-types HUnit kan-extensions
    markdown-unlit mtl network QuickCheck servant servant-client-core
    servant-server tdigest text transformers transformers-compat wai
    warp
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Automatic derivation of querying functions for servant";
  license = stdenv.lib.licenses.bsd3;
}
