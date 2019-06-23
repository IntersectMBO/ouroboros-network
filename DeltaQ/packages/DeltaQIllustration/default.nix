{ mkDerivation, base, containers, data-default, ieee754, lens, mtl
, mwc-random, primitive, statistics, stdenv
}:
mkDerivation {
  pname = "DeltaQIllustration";
  version = "0.1.5";
  src = ./.;
  libraryHaskellDepends = [
    base containers data-default ieee754 lens mtl mwc-random primitive
    statistics
  ];
  license = stdenv.lib.licenses.bsd3;
}
