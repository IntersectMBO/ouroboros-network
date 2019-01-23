{ mkDerivation, aeson, base, fetchgit, stdenv }:
mkDerivation {
  pname = "aeson-options";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/aeson-options";
    sha256 = "12czpbbj3h73cax3r3cmrrrnjm3wa1qwl3gr9zzw5hz9kag7blx0";
    rev = "a25b839b6d2cf1b21fd53906eeaeb3e767cc369a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ aeson base ];
  homepage = "https://github.com/serokell/aeson-options";
  description = "Options to derive FromJSON/ToJSON instances";
  license = stdenv.lib.licenses.mit;
}
