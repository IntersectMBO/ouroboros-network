{ cell, inputs, }:

import "${inputs.self}/release.nix" {
  ouroboros-network = inputs.self;
  supportedSystems = [ inputs.nixpkgs.system ];
}
