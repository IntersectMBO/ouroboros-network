{ inputs, pkgs, lib, project, utils, ghc }:

let

  allTools = {

    "ghc984".cabal = project.projectVariants.ghc984.tool "cabal" "latest";
    "ghc984".cabal-fmt = project.projectVariants.ghc984.tool "cabal-fmt" "latest";
    "ghc984".haskell-language-server = project.projectVariants.ghc984.tool "haskell-language-server" "latest";
    "ghc984".stylish-haskell = project.projectVariants.ghc984.tool "stylish-haskell" "latest";
    "ghc984".fourmolu = project.projectVariants.ghc984.tool "fourmolu" "latest";
    "ghc984".hlint = project.projectVariants.ghc984.tool "hlint" "latest";

    "ghc9102".cabal = project.projectVariants.ghc9102.tool "cabal" "latest";
    "ghc9102".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest"; # cabal-fmt not buildable with ghc9102
    "ghc9102".haskell-language-server = project.projectVariants.ghc9102.tool "haskell-language-server" "latest";
    "ghc9102".stylish-haskell = project.projectVariants.ghc9102.tool "stylish-haskell" "latest";
    "ghc9102".fourmolu = project.projectVariants.ghc9102.tool "fourmolu" "latest";
    "ghc9102".hlint = project.projectVariants.ghc9102.tool "hlint" "latest";

    "ghc9122".cabal = project.projectVariants.ghc9122.tool "cabal" "latest";
    "ghc9122".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest"; # cabal-fmt not buildable with ghc9122
    "ghc9122".haskell-language-server = project.projectVariants.ghc9122.tool "haskell-language-server" "latest";
    "ghc9122".stylish-haskell = project.projectVariants.ghc9122.tool "stylish-haskell" "latest";
    "ghc9122".fourmolu = project.projectVariants.ghc9122.tool "fourmolu" "latest";
    "ghc9122".hlint = project.projectVariants.ghc9122.tool "hlint" "latest";
    "ghc967".cabal = project.projectVariants.ghc967.tool "cabal" "latest";
    "ghc967".cabal-fmt = project.projectVariants.ghc967.tool "cabal-fmt" "latest";
    "ghc967".haskell-language-server = project.projectVariants.ghc967.tool "haskell-language-server" "latest";
    "ghc967".stylish-haskell = project.projectVariants.ghc967.tool "stylish-haskell" "latest";
    "ghc967".fourmolu = project.projectVariants.ghc967.tool "fourmolu" "latest";
    "ghc967".hlint = project.projectVariants.ghc967.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = true;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
        args = [ "--mode" "inplace" ];
      };
      hlint = {
        enable = true;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = true;
        package = pkgs.shellcheck;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.shellcheck
    pkgs.nixpkgs-fmt
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
  ];

  shell = project.shellFor {
    name = "kes-agent-shell-${project.args.compiler-nix-name}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
    ];

    withHoogle = true;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell
