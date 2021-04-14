################################################
# Prepare stylish-haskell to be added as a buildInput
################################################
{ pkgs
, ghcVersion
}:
let
  inherit (pkgs.commonLib) sources;

  planConfig = {
    compiler-nix-name = ghcVersion;
    # get the sources from niv
    src = sources.stylish-haskell;
    modules = [ ];
  };

in
(pkgs.haskell-nix.stackProject planConfig).stylish-haskell.components.exes.stylish-haskell
