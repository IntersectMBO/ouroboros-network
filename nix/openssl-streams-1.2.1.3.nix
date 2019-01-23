{ mkDerivation, base, bytestring, HsOpenSSL, HUnit, io-streams
, network, stdenv, test-framework, test-framework-hunit
}:
mkDerivation {
  pname = "openssl-streams";
  version = "1.2.1.3";
  sha256 = "dc7170e835cf71a132903e2a6ccc976bd2984f9241ea2e4e99a9ece74f868f5f";
  revision = "2";
  editedCabalFile = "1004kgdryflpkp19dv4ikilhcn0xbfc5dsp6v3ib34580pcfj7wy";
  libraryHaskellDepends = [
    base bytestring HsOpenSSL io-streams network
  ];
  testHaskellDepends = [
    base bytestring HsOpenSSL HUnit io-streams network test-framework
    test-framework-hunit
  ];
  description = "OpenSSL network support for io-streams";
  license = stdenv.lib.licenses.bsd3;
}
