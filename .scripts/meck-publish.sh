#!/bin/bash

# Install for Git with alias 'meck-publish':
#   git config alias.meck-publish '!.scripts/meck-publish.sh'

set -e # Abort on first failure, so we don't mess something up

if [ -z "$1" ]; then
    # Missing tag name
    echo "usage: meck-publish <version>" >&2
    exit 129
fi
if [ ! -z "$(git status --short)" ]; then
    # Sanity check
    echo "fatal: dirty repository" >&2
    exit 128
fi

VSN="$1"

# Update version
sed -i "" -e "s/{vsn, .*}/{vsn, \"$VSN\"}/g" src/meck.app.src
sed -i "" -e "s/@version .*/@version $VSN/g" doc/overview.edoc
git add src/meck.app.src
git add doc/overview.edoc

# Commit, tag and push
git commit -m "Version $VSN"
git tag -s "$VSN" -m "Version $VSN"
git push && git push --tags

# Clean and publish package and docs
rm -rf ebin
rm -rf src/**/*.beam
rm -rf test/**/*.beam
rebar3 hex publish
rebar3 hex docs

# Generate and push changelog
github_changelog_generator
git add CHANGELOG.md
git commit -m "Update Changelog for version $VSN"
git push
