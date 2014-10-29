#!/bin/bash

BAU="0"
sigint_handler () {
    echo "Program in pause"
    BAU="1"
    echo "Do you want to quit (y/n)"
    read res
    if [ "${res}" == "y" ]
    then
        echo "Quitting"
        exit 1
    fi
    echo "Going on"
}

set -eu
DATE="$(date +%s)"
DATE_READABLE="$(date -d @${DATE})"
LIST_PACKAGES="$(mktemp)"
LIST_COMMANDS="$(mktemp)"
LIST_COMMANDS2="$(mktemp)"
# echo "LIST_COMMANDS=${LIST_COMMANDS}"
# echo "LIST_PACKAGES=${LIST_PACKAGES}"
trap "rm '${LIST_COMMANDS}' '${LIST_COMMANDS2}' '${LIST_PACKAGES}'" 0

FAST=""
USE_EDITOR=""
while getopts "hfe" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        f)
            FAST="1"
            ;;
        e)
            USE_EDITOR="1"
            ;;
    esac
done
shift $((OPTIND-1))

echo "Computing the list of packages"
aptitude search --disable-columns -F '%p' '~i ?not(~M) ?not(?user-tag(.))' > "${LIST_PACKAGES}"
NUMBER="$(wc -l "${LIST_PACKAGES}"|cut -f1 -d' ')"
LAST_RES="d"

flush_command () {
    local command="${1}"
    shift
    echo "Applying ${command} to ${@}"
	if [ "${command}" == "d" ]
	then
		echo "Removing packages ${@}"
		sudo aptitude purge -y ${@}
	elif [ "${command}" == "a" ]
	then
		echo "Marking ${@} as automatically installed"
		sudo aptitude markauto -y ${@}
	else
		TAG="${DATE_READABLE} (${DATE}):${command}"
		echo "Applying to ${@} the user tag '${TAG}'"
		sudo aptitude add-user-tag "${TAG}" ${@}
	fi
}

flush_commands () {
    # the LIST_COMMANDS is a list package -> command
    # I want a list command -> packages
    # this is what this magical awk command does
    awk 'NR==1 {  } { A[$2]=A[$2]" "$1 } END { for(X in A) print X,substr(A[X],2) }' "${LIST_COMMANDS}" > "${LIST_COMMANDS2}"
    set +e
    trap sigint_handler SIGINT
	while read -u 11 command names
	do
        res="1"
        BAU="1"
        while [ "${res}" != "0" ] && [ "${BAU}" == "1" ]
        do
            # interrupting the command may move BAU to 1
            BAU="0"
            (
                set -e
                flush_command "${command}" ${names}
            )
            res="$?"
        done
	done 11<"${LIST_COMMANDS2}"
	echo -n "" > "${LIST_COMMANDS}"
	read -p "Press Enter to continue"
}

trap "rm '${LIST_COMMANDS}' '${LIST_PACKAGES}'" 0

if [ "${USE_EDITOR}" != "" ]
then
    cp "${LIST_PACKAGES}" "${LIST_COMMANDS}"
    "${EDITOR}" "${LIST_COMMANDS}"
else
    while read -u 10 NAME
    do
	    clear
        echo "--------------- ${NUMBER} left"
	    NUMBER=$((NUMBER-1))
        if [ -n "${FAST}" ]
        then
            echo "${NAME}"
        else
            aptitude show "${NAME}"
        fi
        read -p "> Why is $NAME still here (${LAST_RES}) ? " RES
	    if [ "${RES}" == "f" ]
	    then
		    flush_commands
	    elif [ "${RES}" == "q" ]
	    then
		    break
	    else
		    if [ "${RES}" == "" ]
		    then
			    RES="${LAST_RES}"
		    fi
		    LAST_RES="${RES}"
		    echo "${NAME} ${RES}" >> "${LIST_COMMANDS}"
	    fi
    done 10<"${LIST_PACKAGES}"
fi
flush_commands
