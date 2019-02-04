{}:
let rev = "475d653afdbd8fe3e00ccfd22a30014b0df7aeaa";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
in import (builtins.fetchTarball { inherit url; }) {}
