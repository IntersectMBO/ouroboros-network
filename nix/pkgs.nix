# our packages overlay
localConfig:

pkgs: _: with pkgs; {
  ouroborosNetworkHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      localConfig
      ;
  };

  network-docs = callPackage ./network-docs.nix { };
  consensus-docs = callPackage ./consensus-docs.nix { };
}
