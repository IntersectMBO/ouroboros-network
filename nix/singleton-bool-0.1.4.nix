{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "singleton-bool";
  version = "0.1.4";
  sha256 = "0195c6e2be1e149e5b687ec3be84fd5089b377345fddd333a9d681eacdfafb2a";
  revision = "1";
  editedCabalFile = "0ccd49z9xwa8gr8sclmmn0zc4xq39yyjws4zr6lrw3xjql130nsx";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/phadej/singleton-bool#readme";
  description = "Type level booleans";
  license = stdenv.lib.licenses.bsd3;
}
