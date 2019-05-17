{ config, pkgs, lib, options, ... }:
with lib;
let
  cfg = config.services.byron-proxy;
  name = "byron-proxy";
  stateDir = "/var/lib/byron-proxy";

  byronProxyScripts = (import ../.. {}).byronProxyScripts;
in {
  options.services.byron-proxy = {
    enable = mkEnableOption name;
    environment = mkOption {
      type = types.enum [ "mainnet" "staging" "testnet" ];
      default = "mainnet";
    };
    serverPort = mkOption {
      type = types.int;
      default = 7777;
    };
    serverHost = mkOption {
      type = types.string;
      default = "127.0.0.1";
    };
  };

  config = mkIf cfg.enable {
    users = {
      users.byron-proxy = {
        description     = "byron-proxy";
        group           = "byron-proxy";
        home            = stateDir;
        createHome      = true;
      };
      groups.byron-proxy = {};
    };

    networking.firewall = {
      allowedTCPPorts = lib.optional (cfg.serverHost != "127.0.0.1") cfg.serverPort;
    };

    systemd.services.byron-proxy = {
      description   = "byron proxy service";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = ''${byronProxyScripts.${cfg.environment}}'';
      serviceConfig = {
        User = "byron-proxy";
        Group = "byron-proxy";
        Restart = "always";
        RestartSec = 30;
        StartLimitInterval = 200;
        StartLimitBurst = 5;
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
      };
    };
  };
}
