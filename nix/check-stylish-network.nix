{ runCommand, fd, lib, stylish-haskell }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = ./..;
} ''
  unpackPhase
  cd $sourceRoot
  fd -p io-sim             -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p io-classes         -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p strict-stm         -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p strict-stm         -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p typed-protocols*   -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p network-mux        -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p ouroboros-network* -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
  fd -p cardano-client     -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  echo $? >> $out
''

