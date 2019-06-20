# result/bin/jupyter-lab
# tab completion does not work !
{ compiler ? "ghc864"
, commonLib ? import ../nix/iohk-common.nix
, pkgs ? commonLib.pkgs
, nixTools ? import ../nix/nix-tools.nix {}
, name ? "IOHK"
}:
with pkgs;
with nixTools;
let
  ihaskellExec = nix-tools.exes.ihaskell;
  ihaskellEnv = ghc.withPackages (self: [
    self.ihaskell
    (haskell.lib.doJailbreak self.ihaskell-blaze)
    (haskell.lib.doJailbreak self.ihaskell-diagrams)
    (haskell.lib.doJailbreak self.ihaskell-display)
    ]);

  ihaskellSh = writeScriptBin "ihaskell" ''
    #! ${stdenv.shell}
    export PATH="${stdenv.lib.makeBinPath ([ ihaskellEnv ])}:$PATH"
    ${ihaskellExec}/bin/ihaskell -l $(${ihaskellEnv}/bin/ghc --print-libdir) "$@"'';

  kernelFile = {
    display_name = "Haskell - " + name;
    language = "haskell";
    argv = [
      "${ihaskellSh}/bin/ihaskell"
      "kernel"
      "{connection_file}"
      "+RTS"
      "-M3g"
      "-N2"
      "-RTS"
    ];
    logo64 = "logo-64x64.svg";
  };

  ihaskellKernel = stdenv.mkDerivation {
    name = "ihaskell-kernel";
    phases = "installPhase";
    src = ./haskell.svg;
    installPhase = ''
      mkdir -p $out/kernels/ihaskell_${name}
      cp $src $out/kernels/ihaskell_${name}/logo-64x64.svg
      echo '${builtins.toJSON kernelFile}' > $out/kernels/ihaskell_${name}/kernel.json
    '';
  };

  python3 = pkgs.python3Packages;
  defaultDirectory = "${python3.jupyterlab}/share/jupyter/lab";

  pythonPath = python3.makePythonPath [
        python3.ipykernel
  #      python3.jupyter_contrib_core
  #      python3.jupyter_nbextensions_configurator
  #      python3.tornado
      ];

  jupyterlab = python3.toPythonModule (
        python3.jupyterlab.overridePythonAttrs (oldAttrs: {
          makeWrapperArgs = [
            "--set JUPYTERLAB_DIR ${defaultDirectory}"
            "--set JUPYTER_PATH ${ihaskellKernel}"
            "--set PYTHONPATH ${pythonPath}"
          ];
        })
      );

in
{
jupyterlab-stock= jupyterlab;
}
