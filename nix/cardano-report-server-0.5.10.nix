{
  mkDerivation
, aeson
, aeson-pretty
, base
, bytestring
, case-insensitive
, directory
, exceptions
, fetchgit
, filelock
, filepath
, formatting
, http-types
, lens
, lens-aeson
, lifted-base
, log-warper
, monad-control
, mtl
, network
, optparse-applicative
, parsec
, random
, stdenv
, text
, time
, transformers
, universum
, vector
, wai
, wai-extra
, warp
, wreq
}:
mkDerivation {

pname = "cardano-report-server";
version = "0.5.10";
src = fetchgit {

url = "https://github.com/input-output-hk/cardano-report-server.git";
sha256 = "02n86wbfr3z2xqrc8g8naj0dc5j4644y0l295qzdqlfynmz6a82z";
rev = "9b96874d0f234554a5779d98762cc0a6773a532a";
fetchSubmodules = true;

};
isLibrary = true;
isExecutable = true;
libraryHaskellDepends = [
aeson
aeson-pretty
base
bytestring
case-insensitive
directory
exceptions
filelock
filepath
formatting
http-types
lens
lens-aeson
lifted-base
log-warper
monad-control
mtl
network
optparse-applicative
parsec
random
text
time
transformers
universum
vector
wai
wai-extra
warp
wreq
];
executableHaskellDepends = [
base
directory
filepath
http-types
log-warper
monad-control
mtl
optparse-applicative
parsec
random
universum
wai-extra
warp
];
doHaddock = false;
doCheck = false;
homepage = "https://github.com/input-output-hk/cardano-report-server";
description = "Reporting server for CSL";
license = stdenv.lib.licenses.bsd3;
}
