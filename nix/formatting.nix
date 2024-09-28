pkgs:

let
  inherit (pkgs) lib;
  checkFormatting = tool: script: opts: pkgs.runCommand
    "check-${lib.getName tool}"
    {
      buildInputs = [ pkgs.fd pkgs.which tool ];
      src = ../.;
    } ''
    unpackPhase
    cd $sourceRoot

    bash ${script} ${opts}

    EXIT_CODE=0
    diff -ru $src . || EXIT_CODE=$?

    if [[ $EXIT_CODE != 0 ]]
    then
      echo "*** ${tool.name} found changes that need addressed first"
      exit $EXIT_CODE
    else
      echo $EXIT_CODE >> $out
    fi
  '';
in
{
  stylish-haskell = checkFormatting pkgs.stylish-haskell ../scripts/ci/run-stylish-haskell.sh "-g";
  nixpkgs-fmt = checkFormatting pkgs.nixpkgs-fmt ../scripts/ci/run-nixpkgs-fmt.sh "";
  cabal-gild = checkFormatting pkgs.cabal-gild ../scripts/ci/run-cabal-gild.sh "";
}
