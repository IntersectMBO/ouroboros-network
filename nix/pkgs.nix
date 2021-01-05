# our packages overlay
pkgs: _: with pkgs; {
  ouroborosNetworkHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };

  network-docs = callPackage ./network-docs.nix {};
  consensus-docs = callPackage ./consensus-docs.nix {};
}
