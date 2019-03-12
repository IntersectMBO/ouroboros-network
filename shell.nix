with import ./default.nix {}; with _lib.pkgs;
stdenv.mkDerivation {
  name = "systems-dependencies";
  buildInputs = [ 
    zlib
    haskell.compiler.ghc863
  ];
}