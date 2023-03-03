{ inputs, cell, }: {
  ci = { config, lib, ... }: {
    preset = {
      nix.enable = true;

      github.ci = {
        # Tullia tasks can run locally or on Cicero.
        # When no facts are present we know that we are running locally and vice versa.
        # When running locally, the current directory is already bind-mounted into the container,
        # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
        enable = config.actionRun.facts != { };
        repository = "input-output-hk/ouroboros-network";
        remote = config.preset.github.lib.readRepository
          inputs.cells.cloud.library.actionCiInputName null;
        revision = config.preset.github.lib.readRevision
          inputs.cells.cloud.library.actionCiInputName null;
      };
    };

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = ''
        nix eval .#hydraJobs --apply __attrNames --json |
        nix-systems -i |
        jq 'with_entries(select(.value))' # filter out systems that we cannot build for
      '';
      each.text =
        ''nix build -L .#hydraJobs."$1".required --max-silent-time 1800'';
      skippedDescription =
        lib.escapeShellArg "No nix builder available for this system";
    };

    memory = 1024 * 32;

    nomad = {
      resources.cpu = 10000;

      driver = "exec";
    };
  };
}
