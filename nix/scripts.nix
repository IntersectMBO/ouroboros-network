{
  commonLib,
  customConfig,
  nixTools
}:

let
  # TODO: remove when proxy and old cardano configs no
  # longer required.
  oldCardanoSrc = import ./old-cardano.nix {
    inherit commonLib;
  };
  byronProxy = nixTools.nix-tools.exes.byron-proxy;

in {
  byron = {
    proxy = import ./byron-proxy-scripts.nix {
      inherit commonLib oldCardanoSrc byronProxy customConfig;
    };
    validator = import ./byron-validator-scripts.nix {
      inherit commonLib byronProxy oldCardanoSrc customConfig;
    };
  };
}
