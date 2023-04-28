# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {}, sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [ cabal-install ghcid])));
    overrides = self: super: {
      simple-templates = self.callCabal2nix "simple-templates" ../simple-templates {};
    };
  }
