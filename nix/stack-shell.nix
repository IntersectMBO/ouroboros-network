
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  buildInputs = with pkgs; [ zlib openssl gmp libffi git haskellPackages.happy ];
  ghc = (import ../shell.nix {inherit pkgs;}).ghc;
}
