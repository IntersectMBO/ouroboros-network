{ rev ?  "61deecdc34fc609d0f805b434101f3c8ae3b807a"
}:
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}
