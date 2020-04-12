{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, base64-bytestring
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
        librarySystemDepends = [ pkgs.llvmPackages.bintools pkgs.zlib ];
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
