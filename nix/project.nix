{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (

    { config, pkgs, ... }:

    {
      name = "kes-agent";

      compiler-nix-name = lib.mkDefault "ghc967";

      src = lib.cleanSource ../.;

      flake.variants = {
        ghc967 = { };

        # Disabled other compilers for now, uncomment if needed
        # but the dependencies pinned in the kes-agent.cabal 
        # file are to strict for these versions

        # ghc984.compiler-nix-name = "ghc984";
        # ghc9102.compiler-nix-name = "ghc9102";
        # ghc9122.compiler-nix-name = "ghc9122";
      };

      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

      modules = [
        ({ pkgs, lib, config, ... }:
          let
            # A small helper function to generate post-install
            # scripts for CLI tools to add completions
            # to bash and zsh.
            postInstallCompletion = exeName:
              "  BASH_COMPLETIONS=$out/share/bash-completion/completions\n  ZSH_COMPLETIONS=$out/share/zsh/site-functions\n  mkdir -p $BASH_COMPLETIONS $ZSH_COMPLETIONS\n  $out/bin/${exeName} --bash-completion-script ${exeName} > $BASH_COMPLETIONS/${exeName}\n  $out/bin/${exeName} --zsh-completion-script ${exeName} > $ZSH_COMPLETIONS/_${exeName}\n";
          in
          # we need git at compile time since we use git to generate
            # the version number in the binary
            # Try `nix run .#kes-agent -- --version'
          lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
            packages.kes-agent.components.library.build-tools =
              lib.mkForce [ pkgs.gitMinimal ];

            # Wrap the test binary to have CLI tools in PATH
            # This so that the E2E test can run with the CLI tools
            packages.kes-agent.components.tests.kes-agent-tests.postInstall =
              "  wrapProgram $out/bin/kes-agent-tests --set PATH ${
                  lib.makeBinPath [
                    config.hsPkgs.kes-agent.components.exes.kes-agent
                    config.hsPkgs.kes-agent.components.exes.kes-agent-control
                    config.hsPkgs.kes-agent.components.exes.kes-service-client-demo
                  ]
                }\n";

            # Add completions for all CLI tools
            packages.kes-agent.components.exes.kes-agent.postInstall =
              postInstallCompletion "kes-agent";
            packages.kes-agent.components.exes.kes-agent-control.postInstall =
              postInstallCompletion "kes-agent-control";
            packages.kes-agent.components.exes.kes-service-client-demo.postInstall =
              postInstallCompletion "kes-service-client-demo";
          })
      ];
    }
  );

in

cabalProject
