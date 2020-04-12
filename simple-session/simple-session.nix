{ mkDerivation, base, base64-bytestring, blaze-builder, byteable
, bytestring, containers, cookie, cryptohash, http-types, simple
, stdenv, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "simple-session";
  version = "0.10.1.1";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder byteable bytestring containers
    cookie cryptohash http-types simple transformers wai wai-extra
  ];
  homepage = "http://simple.cx";
  description = "Cookie-based session management for the Simple web framework";
  license = stdenv.lib.licenses.lgpl3;
}
