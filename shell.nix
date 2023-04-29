with import <nixpkgs> {};
# We add our packages to the haskell package set
(haskellPackages.extend (haskell.lib.compose.packageSourceOverrides {
  simple = ./simple;
  simple-templates = ./simple-templates;
  simple-postgresql-orm = ./simple-postgresql-orm;
  simple-session = ./simple-session;
}))
# We call on this set shellFor to drop us into a shell containing the dependencies of frontend and backend:
  .shellFor {
    packages = p: [p.simple p.simple-templates p.simple-postgresql-orm p.simple-session];
    withHoogle = true;
    buildInputs = [ pkgs.cabal-install ];
  }
