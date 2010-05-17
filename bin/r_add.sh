#!/bin/bash

if [[ -z "$*" ]]
then
    echo "r_add.sh <fichier_a_add>"
    exit 1
fi
set "$*"
(
. ~/lib/lib_bash.sh
set $(remove_trailing_slash_KONIX "$1")
file="$(path2file_KONIX "$*")"
rep="$(path2dir_KONIX "$*")"
DST="/home/snake/Prog/.rsync/$file"
SRC="$(pwd)/$*"

if [[ -e "$DST" ]]
then
    echo "Un truc de même nom que '$file' existe déjà dans .rsync"
    exit 1
fi

echo "Deplacement du document"
mv -v "$SRC" "$DST"

echo "lancement de la procédure de link"
r_gen_links.sh

)