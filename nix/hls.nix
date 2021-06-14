################################################
# Prepare Haskell-language-server to be added as a buildInput
################################################
{ pkgs
}:
let
  inherit (pkgs.commonLib) sources;

  planConfig = {
    compiler-nix-name = pkgs.localConfig.ghcVersion;
    # get the sources from niv
    src = sources.hls-released;
    sha256map."https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" = "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
    configureArgs = "--disable-benchmarks";
    modules = [{ enableSeparateDataOutput = true; }];
  };

  # bring all exes into scope
  hlsExes = (pkgs.haskell-nix.cabalProject planConfig).haskell-language-server.components.exes;
in
{
  hls = hlsExes.haskell-language-server;
  hls-wrapper = hlsExes.haskell-language-server-wrapper;
  implicit-hie = pkgs.haskellPackages.implicit-hie;
}
