{ runCommand, fd, nixfmt, haskell-nix }:

runCommand "check-nixfmt" {
  buildInputs = [ fd nixfmt ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };
} ''
  unpackPhase
  cd $sourceRoot
  fd -e nix -X nixfmt -c
  echo $? >> $out
''
