{ compiler ? "ghc864"
, commonLib ? import ../nix/iohk-common.nix
, pkgs ? commonLib.pkgs
, rtsopts ? "-M3g -N2"
, nixTools ? import ../nix/nix-tools.nix {}
}:
with pkgs;
with nixTools;
let
  iHaskellExec = nix-tools.exes.ihaskell;
  iHaskellEnv =  nix-tools.shellFor {
     packages = p: map (x: p."${x}")  [
     "ihaskell"
     "ouroboros-network"
     ];
     };

  jupyter = python3.withPackages (ps: [ ps.jupyter ps.notebook ]);

  ihaskellSh = writeScriptBin "ihaskell-notebook" ''
    #! ${stdenv.shell}
    export PATH="${stdenv.lib.makeBinPath ([ iHaskellExec jupyter ])}:$PATH"
    ${iHaskellExec}/bin/ihaskell install -l $(${iHaskellEnv.ghc}/bin/ghc --print-libdir) && ${jupyter}/bin/jupyter notebook
  '';
in
buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ makeWrapper ];
  paths = [ iHaskellExec jupyter ];
  postBuild = ''
    ln -s ${ihaskellSh}/bin/ihaskell-notebook $out/bin/
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
      fi
    done
  '';
}
