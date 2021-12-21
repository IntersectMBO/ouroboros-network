{ runCommand, fd, nixfmt }:

runCommand "check-nixfmt" {
  buildInputs = [ fd nixfmt ];
  src = ./..;
} ''
  unpackPhase
  cd $sourceRoot
  fd -e nix -X nixfmt -c
  echo $? >> $out
''
