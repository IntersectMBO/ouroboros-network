{ mkDerivation, base, base64-bytestring, bytestring, call-stack
, containers, criterion, deepseq, doctest, doctest-discover
, formatting, hspec, interpolate, microlens, neat-interpolation
, stdenv, text, time, time-locale-compat, vector
}:
mkDerivation {
  pname = "fmt";
  version = "0.6.1.1";
  sha256 = "26220b578d56591cb154cfcb1d98ee8f81c1df97f5955dba91dd00061549d2ad";
  revision = "1";
  editedCabalFile = "13ypmyg0axadzhycfl0g1s73bk9a2myshf38y8dslf3hlg76wbmv";
  libraryHaskellDepends = [
    base base64-bytestring bytestring call-stack containers formatting
    microlens text time time-locale-compat
  ];
  testHaskellDepends = [
    base bytestring call-stack containers doctest hspec
    neat-interpolation text vector
  ];
  testToolDepends = [ doctest-discover ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq formatting interpolate
    text vector
  ];
  homepage = "http://github.com/aelve/fmt";
  description = "A new formatting library";
  license = stdenv.lib.licenses.bsd3;
}
