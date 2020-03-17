############################################################################
# Windows testing bundle
#
# This bundles up the windows build and its dependencies, adds an
# example self-node configuration, and some .BAT files for launching,
# and sets up the Hydra build artifact.
#
############################################################################

{ runCommand
, lib
, zip
, project
, tests ? []
, benchmarks ? []
}:

let

  name = "ouroboros-network-${project.version}-tests-win64";

in runCommand name {
  nativeBuildInputs = [ zip ];
  passthru = { inherit tests benchmarks; };
} ''
  mkdir -p $out/nix-support

  ${lib.concatMapStringsSep "\n" (test: ''
    exe=`cd ${test}/bin; ls -1 *.exe`
    mkdir -p ${test.packageName}
    cp -r ${test}/bin/* ${test.packageName}/
    echo $exe >> ${test.packageName}/tests.bat
    echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> ${test.packageName}/tests.bat
  '') tests}

  ${lib.concatMapStringsSep "\n" (benchmark: ''
    mkdir -p ${benchmark.packageName}
    cp -r ${benchmark}/bin/* ${benchmark.packageName}/
  '') benchmarks}

  chmod -R +w .
  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
