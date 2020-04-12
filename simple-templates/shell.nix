{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, hspec, HUnit
      , scientific, stdenv, text, unordered-containers, vector
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
