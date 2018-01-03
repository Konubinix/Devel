#!/bin/bash -eux

pushd "$(git rev-parse --show-toplevel)"
wget http://git-annex.branchable.com/tips/automatically_adding_metadata/pre-commit-annex \
     -O .git/hooks/pre-commit-annex
chmod +x .git/hooks/pre-commit-annex
git config annex.genmetadata true
# don't know what to extract, so extract everything
git config metadata.extract "$(extract -L | sed 's/ /_/g'|paste -s - -d' ')"
#git config metadata.exiftool "$(exiftool -list | grep '^  '|tr '\r\n' '  ')"
popd
