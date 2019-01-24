{ mkDerivation, base, base-compat, bytestring, HUnit
, markdown-unlit, old-locale, old-time, safe, stdenv
, test-framework, test-framework-hunit, text, time
, time-locale-compat, utf8-string, xml-conduit, xml-types
}:
mkDerivation {
  pname = "feed";
  version = "1.0.1.0";
  sha256 = "87bd055b13c43f54e997f716a33d647c6bc1cdb78337c840d144c0b5fdccd31c";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base base-compat bytestring old-locale old-time safe text time
    time-locale-compat utf8-string xml-conduit xml-types
  ];
  testHaskellDepends = [
    base base-compat HUnit old-time test-framework test-framework-hunit
    text time xml-conduit xml-types
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/bergmark/feed";
  description = "Interfacing with RSS (v 0.9x, 2.x, 1.0) + Atom feeds.";
  license = stdenv.lib.licenses.bsd3;
}
