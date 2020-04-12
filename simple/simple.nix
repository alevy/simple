{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, cmdargs, directory, filepath, hspec
, hspec-contrib, http-types, mime-types, monad-control, mtl
, process, setenv, simple-templates, stdenv, text, transformers
, transformers-base, unordered-containers, vector, wai, wai-extra
}:
mkDerivation {
  pname = "simple";
  version = "0.11.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring blaze-builder bytestring directory
    filepath http-types mime-types monad-control mtl simple-templates
    text transformers transformers-base unordered-containers vector wai
    wai-extra
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring cmdargs directory filepath process
    setenv simple-templates text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base base64-bytestring blaze-builder bytestring directory
    filepath hspec hspec-contrib http-types mime-types monad-control
    mtl simple-templates text transformers transformers-base
    unordered-containers vector wai wai-extra
  ];
  homepage = "http://simple.cx";
  description = "A minimalist web framework for the WAI server interface";
  license = stdenv.lib.licenses.lgpl3;
}
