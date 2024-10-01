{ runCommand, fd, lib, stylish-haskell, haskell-nix }:

runCommand "check-stylish"
{
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "check-stylish";
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
    echo "*** Stylish-haskell found changes that need to be addressed first"
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
