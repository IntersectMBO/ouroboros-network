{ runCommand, fd, lib, stylish-haskell, cabal-fmt, haskell-nix }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell cabal-fmt ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };
} ''
  unpackPhase
  cd $sourceRoot
  bash ./scripts/ci/check-stylish.sh
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
