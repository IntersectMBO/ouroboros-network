# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? { }, sourcesOverride ? { }, withHoogle ? false
, pkgs ? import ./nix { inherit config sourcesOverride; }
, checkTVarInvariant ? false }:
with pkgs;
let
  hsPkgs =
    if checkTVarInvariant
    then ouroborosNetworkHaskellPackagesWithTVarCheck
    else ouroborosNetworkHaskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = hsPkgs.shellFor {
    name = "cabal-dev-shell";

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = [
      cabal
      entr
      fd
      niv
      pkgconfig
      nixfmt
      stylish-haskell
    ];

    tools = {
      # IDE tools
      ghcid = "0.8.7";
      hasktags = "0.71.2";
      haskell-language-server = "1.8.0.0";
      # Draw graph of module dependencies
      graphmod = "1.4.4";
      # Profiling tools
      profiteur = "0.4.6.0";
      eventlog2html = "0.9.2";
      hp2pretty = "0.10";
    };

    shellHook = ''
      export LANG="en_US.UTF-8"
    '' + lib.optionalString
      (pkgs.glibcLocales != null && stdenv.hostPlatform.libc == "glibc") ''
        export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
      '';

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [ niv ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in shell // { inherit devops; }
