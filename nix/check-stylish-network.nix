{ runCommand, fd, lib, stylish-haskell, haskell-nix }:

runCommand "check-stylish-network" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };
} ''
  unpackPhase
  cd $sourceRoot
  bash ./scripts/ci/check-stylish-network.sh
  diff -ru $src .

  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    diff -ru $src .
    echo "*** Stylish-haskell found changes that need addressed first"
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
