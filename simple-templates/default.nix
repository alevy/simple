{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./simple-templates.nix { }
