{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, nixpkgsArgs ? {
    config = { allowUnfree = false; inHydra = true; };
  }
}:
let nixpkgs = import ./nix/nixpkgs.nix {};
    rlib = import (nixpkgs.path + "/pkgs/top-level/release-lib.nix")
    ouroboros-network = import ./default.nix {};
    platforms = {
      ouroboros-network = supportedSystems;
    };
in rlib.mapTestOn platforms // ouroboros-network
