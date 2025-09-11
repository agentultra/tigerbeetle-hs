#!/bin/env sh
set -e

# Upload haddocks to hackage after creating a release.

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

# assumes cabal 2.4 or later
cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload -d --publish $dir/*-docs.tar.gz
