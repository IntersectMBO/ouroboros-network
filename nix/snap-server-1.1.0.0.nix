{ mkDerivation, attoparsec, base, base16-bytestring, blaze-builder
, bytestring, bytestring-builder, case-insensitive, clock
, containers, criterion, deepseq, directory, filepath, HsOpenSSL
, http-common, http-streams, HUnit, io-streams, io-streams-haproxy
, lifted-base, monad-control, mtl, network, old-locale
, openssl-streams, parallel, QuickCheck, random, snap-core, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, threads, time, transformers, unix, unix-compat, vector
}:
mkDerivation {
  pname = "snap-server";
  version = "1.1.0.0";
  sha256 = "249ea390a4e54899b310c0dd13b91af007a2b685bd0d9769c3e208dd914d7c6f";
  revision = "3";
  editedCabalFile = "0a9d3nqb5rvgm25nak68lp6yj9m6cwhbgdbg5l7ib5i2czcg7yjh";
  configureFlags = [ "-fopenssl" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base blaze-builder bytestring bytestring-builder
    case-insensitive clock containers filepath HsOpenSSL io-streams
    io-streams-haproxy lifted-base mtl network old-locale
    openssl-streams snap-core text time unix unix-compat vector
  ];
  testHaskellDepends = [
    attoparsec base base16-bytestring blaze-builder bytestring
    bytestring-builder case-insensitive clock containers deepseq
    directory filepath HsOpenSSL http-common http-streams HUnit
    io-streams io-streams-haproxy lifted-base monad-control mtl network
    old-locale openssl-streams parallel QuickCheck random snap-core
    test-framework test-framework-hunit test-framework-quickcheck2 text
    threads time transformers unix unix-compat vector
  ];
  benchmarkHaskellDepends = [
    attoparsec base blaze-builder bytestring bytestring-builder
    criterion io-streams io-streams-haproxy snap-core vector
  ];
  homepage = "http://snapframework.com/";
  description = "A web server for the Snap Framework";
  license = stdenv.lib.licenses.bsd3;
}
