
{ ouroboros-network ? { outPath = ./.; rev = "acdef"; }, ... }@args:
let
  commonLib = import ./nix/iohk-common.nix;
  default = import ./default.nix {};

in commonLib.nix-tools.release-nix {
  package-set-path = ./nix/nix-tools.nix;
  _this = ouroboros-network;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "typed-protocols"
               "typed-protocols-cbor"
               "network-mux"
               "ouroboros-network"
               "ouroboros-network-testing"
               "ouroboros-consensus"
               "io-sim"
               "io-sim-classes"
             ];

  # The set of jobs we consider crutial for each CI run.
  # if a single one of these fails, the build will be marked
  # as failed.
  #
  # The names can be looked up on hydra when in doubt.
  #
  # custom jobs will follow their name as set forth in
  # other-packages.
  #
  # nix-tools packages are prefixed with `nix-tools` and
  # follow the following naming convention:
  #
  #   namespace                      optional cross compilation prefix                 build machine
  #   .-------.                              .----------------.                .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.[x86_64-pc-mingw-]$pkg.$component.{x86_64-linux,x86_64-darwin}
  #             '--------------------------'                    '-------------'
  #                 component type                          cabal pkg and component*
  #
  # * note that for `libs`, $component is empty, as cabal only
  #   provides a single library for packages right now.
  # * note that for `exes`, $component is also empty, because it
  #   it provides all exes under a single result directory.
  #   To  specify a single executable component to build, use
  #   `cexes` as component type.
  # 
  # Aggregates of all components of a given type accross all
  # specified packages are also available:
  #
  #   namespace                              component aggregate type
  #   .-------.                            .--------------------------.
  #   nix-tools.[x86_64-pc-mingw-]packages-{libs,exes,tests,benchmarks}.{x86_64-linux,x86_64-darwin}
  #             '----------------'                                      '--------------------------'
  #     optional cross compilation prefix                                      build machine

  #
  # Example:
  #
  #   nix-tools.libs.ouroboros-consensus.x86_64-darwin -- will build the ouroboros-consensus library on and for macOS
  #   nix-tools.libs.x86_64-pc-mingw32-ouroboros-network.x86_64-linux -- will build the ouroboros-network library on linux for windows.
  #   nix-tools.tests.ouroboros-consensus.test-crypto.x86_64-linux -- will build and run the test-crypto from the
  #                                                          ouroboros-consensus package on linux.
  #   nix-tools.packages-tests.x86_64-linux -- will build and run all the tests from all the packages specified above.

  extraBuilds = {
    tests = default.tests;
    network-pdf-wip = default.network-pdf-wip;
    network-pdf = default.network-pdf;
  };
  # so that the shell is also built for darwin:
  builds-on-supported-systems = [ "shell" ];
  # FIXME: some jobs currently don't build:
  disabled-jobs = [
    # "Please use win32/Makefile.gcc instead.":
    "nix-tools.tests.x86_64-pc-mingw32-ouroboros-network.cddl.x86_64-linux"
    # hangs at Socket tests:
    "nix-tools.tests.x86_64-pc-mingw32-ouroboros-network.tests.x86_64-linux"
    # 'Network.Socket.bind: permission denied (Operation not permitted)' at Socket tests:
    "nix-tools.tests.ouroboros-network.tests.x86_64-darwin"
    # 'Storage.HasFS.HasFS' test failing:
    "nix-tools.tests.ouroboros-consensus.test-storage.x86_64-darwin"
    "nix-tools.tests.x86_64-pc-mingw32-ouroboros-consensus.test-storage.x86_64-linux"
  ];
  # The required jobs that must pass for ci not to fail:
  required-name = "ouroboros-network-required-checks";
  required-targets = jobs: [
    # targets are specified using above nomenclature:
    jobs.nix-tools.packages-libs.x86_64-linux
    jobs.nix-tools.packages-tests.x86_64-linux
    jobs.nix-tools.packages-exes.x86_64-linux
    
    jobs.nix-tools.packages-libs.x86_64-darwin
    jobs.nix-tools.packages-tests.x86_64-darwin
    jobs.nix-tools.packages-exes.x86_64-darwin

    # windows cross compilation targets
    jobs.nix-tools.x86_64-pc-mingw32-packages-libs.x86_64-linux
    jobs.nix-tools.x86_64-pc-mingw32-packages-tests.x86_64-linux
    jobs.nix-tools.x86_64-pc-mingw32-packages-exes.x86_64-linux

    # additional required jobs:
    jobs.network-pdf
    jobs.shell.x86_64-linux
    
    # FIXME: https://github.com/NixOS/nix/issues/2311
    # jobs.shell.x86_64-darwin
  ];
} (builtins.removeAttrs args ["ouroboros-network"])
