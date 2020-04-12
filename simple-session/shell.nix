{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, blaze-builder
      , byteable, bytestring, containers, cookie, cryptohash, http-types
      , simple, stdenv, transformers, wai, wai-extra
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
