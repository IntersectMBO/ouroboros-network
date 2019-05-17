{ config, pkgs, lib, options, ... }:
with lib;
let
  cfg = config.services.byron-validator;
  name = "byron-validator";

  byronProxyScripts = (import ../.. {}).byronValidatorScripts;
in {
  options.services.byron-validator = {
    enable = mkEnableOption name;
    environment = mkOption {
      type = types.enum [ "mainnet" "staging" "testnet" ];
      default = "mainnet";
    };
    proxyPort = mkOption {
      type = types.int;
      default = 7777;
    };
    proxyHost = mkOption {
      type = types.string;
      default = "127.0.0.1";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.byron-validator = {
      description   = "byron validator service";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = let
        customConfig = {
          proxyHost = cfg.proxyHost;
          proxyPort = cfg.proxyPort;
        };
        byronScripts = (import ../.. { inherit customConfig; }).scripts.byron;
      in ''${byronScripts.validator.${cfg.environment}}'';
      serviceConfig = {
        Restart = "always";
        RestartSec = 30;
        StartLimitInterval = 200;
        StartLimitBurst = 5;
        KillSignal = "SIGINT";
        PrivateTmp = true;
      };
    };
  };
}
