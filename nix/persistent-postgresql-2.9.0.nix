{ mkDerivation, aeson, base, blaze-builder, bytestring, conduit
, containers, monad-logger, persistent, postgresql-libpq
, postgresql-simple, resource-pool, resourcet, stdenv, text, time
, transformers, unliftio-core
}:
mkDerivation {
  pname = "persistent-postgresql";
  version = "2.9.0";
  sha256 = "bd029ca877f9536398e9703e5886731059dbcbd7015cdc470b54727e7e5b14e7";
  revision = "1";
  editedCabalFile = "0xrnww7n6kwr2371fj5xklslbx0114yj3pxcpdzwalmin5wm8vah";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring conduit containers monad-logger
    persistent postgresql-libpq postgresql-simple resource-pool
    resourcet text time transformers unliftio-core
  ];
  homepage = "http://www.yesodweb.com/book/persistent";
  description = "Backend for the persistent library using postgresql";
  license = stdenv.lib.licenses.mit;
}
