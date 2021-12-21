# ###############################################
# Prepare Haskell-language-server to be added as a buildInput
################################################
{ pkgs }:
let
  inherit (pkgs.commonLib) sources;

  planConfig = {
    compiler-nix-name = pkgs.localConfig.ghcVersion;
    # get the sources from niv
    src = sources.hls-released;
    configureArgs = "--disable-benchmarks";
    modules = [{ enableSeparateDataOutput = true; }];
  };

  # bring all exes into scope
  hlsExes = (pkgs.haskell-nix.cabalProject
    planConfig).haskell-language-server.components.exes;
in {
  hls = hlsExes.haskell-language-server;
  hls-wrapper = hlsExes.haskell-language-server-wrapper;
  implicit-hie = pkgs.haskellPackages.implicit-hie;
}
