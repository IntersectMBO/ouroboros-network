{ commonLib
, oldCardanoSrc
, byronProxy
, customConfig
}:

let
  config = if customConfig != {} then customConfig else {
    proxyHost = "127.0.0.1";
    proxyPort = 7777;
  };
  pkgs = commonLib.pkgs;
  oldCardano = import oldCardanoSrc {};
  oldCardanoLib = import (oldCardanoSrc + "/lib.nix");
  environments = oldCardanoLib.environments;
  loggingConfig = ../byron-proxy/cfg/logging.yaml;
  mkTopology = relay: pkgs.writeText "topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  mkProxyScript = environment: let
      envConfig = environments.${environment};
    in with config; pkgs.writeScript "byron-proxy-${environment}" ''
    exec ${byronProxy}/bin/byron-proxy +RTS -T -RTS --database-path db-byron-proxy-${environment} --index-path index-byron-proxy-${environment} --configuration-file ${configuration}/lib/configuration.yaml --configuration-key ${envConfig.confKey} --server-host ${proxyHost} --server-port ${builtins.toString proxyPort} --topology ${mkTopology envConfig.relays} --logger-config ${loggingConfig}
  '';
  configuration = oldCardano.cardano-sl-config;

in {
  mainnet = mkProxyScript "mainnet";
  staging = mkProxyScript "staging";
  testnet = mkProxyScript "testnet";
}
