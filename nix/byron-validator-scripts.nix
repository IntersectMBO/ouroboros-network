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
  loggingConfig = ../byron-proxy/cfg/logging-validator.yaml;
  configuration = oldCardano.cardano-sl-config;
  genesisFiles = let
    fixGenesis = environment: file: pkgs.runCommand "fix-genesis-${environment}" { input = file; } ''
      cat ${file} | ${byronProxy}/bin/parse-genesis > $out
    '';
  in {
    mainnet = fixGenesis "mainnet" "${configuration}/lib/mainnet-genesis.json";
    staging = fixGenesis "staging" "${configuration}/lib/mainnet-genesis-dryrun-with-stakeholders.json";
    testnet = fixGenesis "testnet" "${configuration}/lib/testnet-genesis.json";
  };
  mkValidatorScript = environment: with config; pkgs.writeScript "byron-validator-${environment}" ''
    exec ${byronProxy}/bin/validator +RTS -T -RTS --server-host ${proxyHost} --server-port ${builtins.toString proxyPort} --logger-config ${loggingConfig} --override-genesis-json ${genesisFiles.${environment}}
  '';

# TODO: add more environments when it isn't hard-coded to mainnet
in {
  mainnet = mkValidatorScript "mainnet";
  staging = mkValidatorScript "staging";
  testnet = mkValidatorScript "testnet";
}
