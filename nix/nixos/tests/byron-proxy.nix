{ pkgs, ... }:

{
  name = "byron-proxy-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [
        ../.
      ];
      services.byron-proxy = {
        enable = true;
        proxyHost = "0.0.0.0";
        proxyPort = 7777;
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("byron-proxy.service");
    $machine->waitForOpenPort(7777);
  '';

}
