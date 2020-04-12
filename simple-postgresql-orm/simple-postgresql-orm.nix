{ mkDerivation, base, bytestring, directory, filepath
, postgresql-orm, postgresql-simple, resource-pool, simple, stdenv
, transformers
}:
mkDerivation {
  pname = "simple-postgresql-orm";
  version = "0.9.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring directory filepath postgresql-orm postgresql-simple
    resource-pool simple transformers
  ];
  homepage = "http://simple.cx";
  description = "Connector package for integrating postgresql-orm with the Simple web framework";
  license = stdenv.lib.licenses.lgpl3;
}
