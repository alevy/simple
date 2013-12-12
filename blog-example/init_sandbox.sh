#!/bin/bash

cabal sandbox init
cabal sandbox add-source ../simple
cabal sandbox add-source ../simple-templates
cabal sandbox add-source ../simple-session
cabal sandbox add-source ../simple-postgresql-orm
