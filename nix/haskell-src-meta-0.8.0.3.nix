{ mkDerivation, base, haskell-src-exts, HUnit, pretty, stdenv, syb
, template-haskell, test-framework, test-framework-hunit
, th-orphans
}:
mkDerivation {
  pname = "haskell-src-meta";
  version = "0.8.0.3";
  sha256 = "8473e3555080860c2043581b398dbab67319584a568463b074a092fd4d095822";
  revision = "2";
  editedCabalFile = "0dp5v0yd0wgijzaggr22glgjswpa65hy84h8awdzd9d78g2fjz6c";
  libraryHaskellDepends = [
    base haskell-src-exts pretty syb template-haskell th-orphans
  ];
  testHaskellDepends = [
    base haskell-src-exts HUnit pretty template-haskell test-framework
    test-framework-hunit
  ];
  description = "Parse source to template-haskell abstract syntax";
  license = stdenv.lib.licenses.bsd3;
}
