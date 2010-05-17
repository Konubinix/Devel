#!/bin/bash
echo "Suppression des fichiers non liés"

(
    cd ~/Prog
    OK=0
    for i in .rsync/*
    do
	test -e $(echo "$i" | sed 's-^.rsync/--')
	if [[ $? != 0 ]]
	then
	    echo $i "n'est pas lié"
	    echo "Supprimer le fichier? o/O : oui, autres : non"
	    read resp
	    if [[ $resp == "o" || $resp == "O" ]]
	    then
		rm -rfv $i
	    else
		echo "  abort"
	    fi
	fi
    done

)