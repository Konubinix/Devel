#!/bin/bash

if [[ -z "$*" ]]
then
    echo "r_del.sh <fichier_a_del>"
    exit 1
fi

(
    cd ~/Prog
    . ~/lib/lib_bash.sh
    . ~/lib/global.sh
    set $(remove_trailing_slash_KONIX "$*")
    file="$(path2file_KONIX "$*")"
    rep="$(path2dir_KONIX "$*")"
    SRC="$PROG/.rsync/$file"
    DST="$PROG/$file"

    test=$(ls -F "$DST" | grep "@$")
    if [[ $test != "" ]]
    then
	unlink "$DST"
    fi
    
    if [[ ! -e "$SRC" ]]
    then
	echo "Impossible de trouver '$file' dans le rep .rsync" 
	exit 1
    fi

    echo "Deplacement du document"
    mv -v "$SRC" "$DST"

    # echo "lancement de la procédure de link"
    # r_gen_links.sh

)