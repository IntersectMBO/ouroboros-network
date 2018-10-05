let
  fetchNixpkgs = import ./nix/fetchNixPkgs.nix {};
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , nixpkgsArgs ? {
      config = { allowUnfree = false; inHydra = true; };
    }
  }:

with (import (fetchNixpkgs  + "/pkgs/top-level/release-lib.nix")
  { inherit supportedSystems nixpkgsArgs; });

let nixpkgs = import ./nix/nixpkgs.nix {};
    ouroboros-network = import ./default.nix {};
    platforms = {
      ouroboros-network = supportedSystems;
    };
in mapTestOn platforms // ouroboros-network // { inherit nixpkgs; }
