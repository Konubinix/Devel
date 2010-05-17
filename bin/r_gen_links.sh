#!/bin/bash
echo "Generation des liens vers le rep .rsync"
(
    cd ~/Prog
    OK=0
    for i in $(ls .rsync)
    do
	test -e $i
	if [[ $? != 0 ]]
	then
	    echo $i "n'est pas lié"
	    OK=$(( OK + 1 ))
	    a_lier[$OK]="$i"
	fi
    done

    if [[ $OK == 0 ]]
    then
	echo "Tout baigne !"
	exit 0
    fi

    echo "Créer les liens? o/O : oui, autres : non"
    read resp
    if [[ $resp == "o" || $resp == "O" ]]
    then
	for i in  ${!a_lier[*]}
	do
	    echo "Link de " ${a_lier[$i]}
	    ln -sv "./.rsync/${a_lier[$i]}" ${a_lier[$i]}
	done
    fi
)