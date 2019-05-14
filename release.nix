let
  commonLib = import ./nix/iohk-common.nix;
  # Path of nix-tools jobs that we want to evict from release.nix:
  disabled = [
    # FIXME: those tests freeze on darwin hydra agents:
    ["nix-tools" "tests" "ouroboros-consensus" "test-storage" "x86_64-darwin"]
  ];
in
{ ... }@args:
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  package-set-path = ./.;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "typed-protocols"
               "ouroboros-network"
               "ouroboros-network-testing"
               "ouroboros-consensus"
               "io-sim"
               "io-sim-classes"
               "byron-proxy"
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
  #   .-------.                              .-----------------.                .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.{x86_64-pc-mingw-,}$pkg.$component.{x86_64-linux,x86_64-darwin}
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
  # Example:
  #
  #   nix-tools.libs.ouroboros-consensus.x86_64-darwin -- will build the ouroboros-consensus library on and for macOS
  #   nix-tools.libs.x86_64-pc-mingw32-ouroboros-network.x86_64-linux -- will build the ouroboros-network library on linux for windows.
  #   nix-tools.tests.ouroboros-consensus.test-crypto.x86_64-linux -- will build and run the test-crypto from the
  #                                                          ouroboros-consensus package on linux.

  # The required jobs that must pass for ci not to fail:
  required-name = "ouroboros-network-required-checks";
  required-targets = jobs: [
    # targets are specified using above nomenclature:
    jobs.nix-tools.tests.ouroboros-consensus.test-consensus.x86_64-linux
    jobs.nix-tools.tests.ouroboros-consensus.test-crypto.x86_64-linux
    jobs.nix-tools.tests.ouroboros-consensus.test-storage.x86_64-linux
    jobs.nix-tools.tests.ouroboros-network.tests.x86_64-linux
    jobs.nix-tools.tests.typed-protocols.tests.x86_64-linux
    jobs.nix-tools.tests.io-sim.tests.x86_64-linux

    jobs.nix-tools.exes.ouroboros-consensus.x86_64-linux
    jobs.nix-tools.exes.byron-proxy.x86_64-linux
  ];
} args)
