################################################
# Prepare stylish-haskell to be added as a buildInput
################################################
{ pkgs
}:
let
  inherit (pkgs.commonLib) sources;

  planConfig = {
    compiler-nix-name = pkgs.localConfig.ghcVersion;
    # get the sources from niv
    src = sources.stylish-haskell;
    modules = [ ];
  };

in
(pkgs.haskell-nix.stackProject planConfig).stylish-haskell.components.exes.stylish-haskell
