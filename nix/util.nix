{ haskell-nix }:

with haskell-nix.haskellLib; {

  inherit selectProjectPackages collectChecks' collectComponents';

  inherit (extra) recRecurseIntoAttrs;

}
