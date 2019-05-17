{ pkgs, ... }:

{
  name = "byron-proxy-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [
        (import ../.)
      ];
      services.byron-proxy = {
        enable = true;
        serverHost = "0.0.0.0";
        serverPort = 7777;
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("byron-proxy.service");
    $machine->waitForOpenPort(7777);
  '';

}
