{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:

rec {
  simple-templates = import ./simple-templates { inherit nixpkgs compiler; };
  simple = import ./simple { inherit nixpkgs compiler simple-templates; };
  simple-postgresql-orm = import ./simple-postgresql-orm { inherit nixpkgs compiler simple; };
  simple-session = import ./simple-session { inherit nixpkgs compiler simple; };
}
