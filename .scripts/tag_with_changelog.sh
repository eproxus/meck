#!/bin/bash
set -e # Abort on first failure, so we don't mess something up

if [ -z "$1" ]; then
    echo "usage: git tag-cl <tag>" >&2
    exit 129
fi
if [ ! -f CHANGELOG ]; then
    echo "fatal: CHANGELOG missing" >&2
    exit 128
fi
if [ ! -z "$(git status)" ]; then
    echo "fatal: dirty repository" >&2
    exit 128
fi

CHANGELOG=$(cat CHANGELOG)

# Add tag with CHANGELOG and empty CHANGELOG afterwards
echo echo "" > CHANGELOG
echo sed -i "" -e "s/{vsn, .*}/{vsn, \"$1\"}/g" src/meck.app.src

echo git add src/meck.app.src
echo git add CHANGELOG

echo git commit -m "Version $1"

echo git tag -s $1 -m "Version $

$CHANGELOG"

echo git push && echo git push --tags
