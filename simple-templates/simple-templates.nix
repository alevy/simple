{ mkDerivation, aeson, attoparsec, base, hspec, HUnit, scientific
, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "simple-templates";
  version = "0.9.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base scientific text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base hspec HUnit scientific vector
  ];
  homepage = "http://simple.cx";
  description = "A basic template language for the Simple web framework";
  license = stdenv.lib.licenses.lgpl3;
}
