# ###############################################
# Prepare Haskell-language-server to be added as a buildInput
################################################
{ pkgs }:
let
  inherit (pkgs.commonLib) sources;

  planConfig = {
    compiler-nix-name = pkgs.localConfig.ghcVersion;
    # get the sources from niv
    src = pkgs.applyPatches {
      name = "hls";
      src = sources.hls-released;
      # see https://github.com/input-output-hk/haskell.nix/pull/1457
      patches = [
        (pkgs.fetchpatch {
          url =
            "https://raw.githubusercontent.com/input-output-hk/haskell.nix/4ee7270856a6ba4617c7e51a6c619f365faad894/patches/ghcide-1.7-unboxed-tuple-fix-issue-1455.patch";
          sha256 = "118cy4jkfhhwxr0bc0g56l4vyp3svdck9cw1r8jc9nlv898iflyp";
          stripLen = 1;
          extraPrefix = "ghcide/";
        })
      ];
    };
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
