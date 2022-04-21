{ runCommand, haskell-nix, fd, lib, stylish-haskell }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-src";
    src = ./..;
  };
} ''
  mkdir $out
  unpackPhase
  cd $sourceRoot
  export LC_ALL=C.UTF-8
  bash ./scripts/ci/check-stylish-network.sh
  bash ./scripts/ci/check-stylish.sh
''
