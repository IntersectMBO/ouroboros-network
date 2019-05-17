{ pkgs, ... }:

{
  name = "byron-proxy-validator-test";
  nodes = {
    proxy = { config, pkgs, ... }: {
      imports = [
        (import ../.)
      ];
      services.byron-proxy = {
        enable = true;
        proxyHost = "0.0.0.0";
        proxyPort = 7777;
      };
    };
    validator = { nodes, config, pkgs, ... }: {
      imports = [
        (import ../.)
      ];
      services.byron-validator = {
        enable = true;
        proxyHost = nodes.proxy.config.networking.primaryIPAddress;
        proxyPort = 7777;
      };
    };
  };
  testScript = ''
    startAll
    $proxy->waitForUnit("byron-proxy.service");
    $proxy->waitForOpenPort(7777);
    $validator->waitForUnit("byron-validator.service");
  '';

}
