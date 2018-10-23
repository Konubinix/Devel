#!/bin/bash -eux

echo '###### adding to git annex'
git annex add "$@"
echo '##### setting the metadata to delete'
git annex metadata --force -s ack=delete "$@"
echo ' ####### removing the file'
rm -rf "$@"
