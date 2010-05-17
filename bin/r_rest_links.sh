#!/bin/bash
echo "Correction des liens : Suppression des inutiles et rennomage des fichiers suivant les noms des liens"

(
    . ~/lib/lib_bash.sh
    cd ~/Prog
    OK=0
    for i in $(ls -F | grep '@$' | sed 's/@$//')
    do
	# test que le fichier est bien lié quelque part
	test -e "$i"
	if [[ $? != 0 ]]
	then
	    # le fichier pointe vers rien du tout
	    echo $i "est lié dans le vide"
	    OK=$(( OK + 1 ))
	    a_delier[$OK]="$i"
	else
	    # le fichier pointe vers un fichier, on vérifie qu'il a le
	    # même nom, et on rennome le fichier pointé s'il ne l'a
	    # pas
	    link=$(readlink $i)
	    rel_link=$(path2file_KONIX "$link")
	    if [[ "$i" != "$rel_link" ]]
	    then
		# le fichier pointé n'a pas le même nom
		echo "Le fichier $i pointe vers $(readlink $i) au lieu de .rsync/$i"
		echo "Effectuer le rennomage? o/O : oui, autres : non"
		read resp
		if [[ $resp == "o" || $resp == "O" ]]
		then
		    cp -fLrv "$i" ".rsync/$i"
		    rm -frv "$link" "$i"
		    ln -sv  ".rsync/$i" "$i"
		else
		    echo "  abort"
		fi
	    fi
	fi

    done

    if [[ $OK == 0 ]]
    then
	echo "Tout baigne !"
	exit 0
    fi

    echo "Détruire ces liens ? o/O : oui, autres : non"
    read resp
    if [[ $resp == "o" || $resp == "O" ]]
    then
	for i in  ${!a_delier[*]}
	do
	    echo "Destruction de " ${a_delier[$i]}
	    unlink ${a_delier[$i]}
	done
    fi
)