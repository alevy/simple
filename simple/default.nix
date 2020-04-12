{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883", simple-templates ? import ../simple-templates { inherit nixpkgs compiler; } }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./simple.nix { inherit simple-templates; }
