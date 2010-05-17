#!/bin/bash

moi="${HOME}/Prog/.rsync/"
args="-avztuh --progress"

commit=0
update=0
list=0
delete=0

while getopts "culd" optionName; do
    case "$optionName" in
#	h) printHelpAndExit 0;;
	d) delete=1;;
	c) commit=1;;
	u) update=1;;
	l) list=1;;
	# m) maxCpuUsage="$OPTARG";;
	# [?]) printErrorHelpAndExit "$badOptionHelp";;
    esac
done

# echo $commit
# echo $update
# echo $list
# echo $delete

if [[ ( $commit == 0 ) && ( $update == 0 ) ]]
then
    echo "Need commit or update"
    exit 1
fi

if [[ ( $commit == 1 ) && ( $update == 1 ) ]]
then
    echo "Need only commit or only update"
    exit 1
fi

if [[ $commit == 1 ]]
then
    src="$moi"
    dest="lourys@ensisun.imag.fr:rsync/"
fi

if [[ $update == 1 ]]
then
    dest="$moi"
    src="lourys@ensisun.imag.fr:rsync/"
fi

if [[ $delete == 1 ]]
then
    args="$args --delete"
fi

if [[ $list == 1 ]]
then
    args="$args --list-only"
fi


echo "Lancement de rsync $args $src $dest"
rsync $args $src $dest
    
if [[ $update == 1 ]]
then
    r_gen_links.sh
fi