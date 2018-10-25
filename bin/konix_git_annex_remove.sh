#!/bin/bash -eux

echo '###### adding to git annex'
git annex add "$@"
echo '##### setting the metadata to delete'
git annex metadata --force -s ack=delete "$@"
echo ' ####### git annex dropping the file'
git annex drop --force "$@"
echo ' ####### removing the file'
rm -rf "$@"
