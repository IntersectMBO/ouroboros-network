{ runCommand, fd, lib, stylish-haskell }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = ./..;
} ''
  unpackPhase
  cd $sourceRoot
  fd -p ouroboros-consensus* -e hs -E Setup.hs -E Ouroboros/Consensus/Mempool/TxLimits.hs -X stylish-haskell -c .stylish-haskell.yaml -i
  echo $? >> $out
''
