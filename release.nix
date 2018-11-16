let
  fixedLib     = import ./lib.nix;
  fixedNixpkgs = fixedLib.iohkNix.nixpkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , fasterBuild ? false
  , skipPackages ? []
  , nixpkgsArgs ? {
      config = { allowUnfree = false; inHydra = true; };
    }
  }:

let
  inherit (pkgs) lib;
  makeRelease = fun: platforms: builtins.mapAttrs (key: value: (fun value).${key}) platforms;
  compilers = [ "ghc843" "ghc861" ];
  pkgs = import fixedNixpkgs { config = {}; };
  platforms = {
    ouroboros-network = supportedSystems;
  };
  mapped = makeRelease (system: import ./. { inherit system; }) platforms;
  makeTestRuns = system:
  let
    pred = name: value: (fixedLib.isLocalPackage name);
    f = name: value: value.testrun;
    ouroborosPkgs = import ./. { inherit system; };
  in lib.mapAttrs f (lib.filterAttrs pred ouroborosPkgs.haskellPackages);

in pkgs.lib.fix (jobsets: mapped // {
  inherit (pkgs) cabal2nix;
  # the result of running every cardano test-suite on 64bit linux
  ouroboros-network-tests.x86_64-linux = makeTestRuns "x86_64-linux";
  # hydra will create a special aggregate job, that relies on all of these sub-jobs passing
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "ouroboros-network-required-checks";
    constituents =
      let
        allLinux = x: map (system: x.${system}) [ "x86_64-linux" ];
        all = x: map (system: x.${system}) supportedSystems;
      in
    [
      (builtins.concatLists (map pkgs.lib.attrValues (all jobsets.ouroboros-network-tests)))
      (all jobsets.ouroboros-network)
    ];
  });
})

