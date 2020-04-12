{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883",
  simple-templates ? import ../simple-templates {inherit nixpkgs compiler; },
  simple ? import ../simple {inherit nixpkgs compiler simple-templates; }
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./simple-session.nix { inherit simple; }
