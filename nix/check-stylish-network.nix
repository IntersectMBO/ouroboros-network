{ runCommand, fd, lib, stylish-haskell, git }:

runCommand "check-stylish-network" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell git ];
  src = ./..;
} ''
  unpackPhase
  cd $sourceRoot
  export LC_ALL=C.UTF-8
  fd -p io-sim -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p io-classes -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p strict-stm -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p typed-protocols -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p network-mux -e hs -E Setup.hs -E network-mux/src/Network/Mux/Bearer/Pipe.hs -E network-mux/src/Network/Mux/Channel.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p ouroboros-network* -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
  fd -p cardano-client -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i

  git diff >> $out
''
