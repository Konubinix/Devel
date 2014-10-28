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
# echo "LIST_COMMANDS=${LIST_COMMANDS}"
# echo "LIST_PACKAGES=${LIST_PACKAGES}"
trap "rm '${LIST_COMMANDS}' '${LIST_PACKAGES}'" 0

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
    local name="${1}"
    local command="${2}"
    echo "Applying ${command} to ${name}"
	if [ "${command}" == "d" ]
	then
		echo "Removing packages ${name}"
		sudo aptitude purge -y "${name}"
	elif [ "${command}" == "a" ]
	then
		echo "Marking ${name} as automatically installed"
		sudo aptitude markauto -y "${name}"
	else
		TAG="${DATE_READABLE} (${DATE}):${command}"
		echo "Applying to ${name} the user tag '${TAG}'"
		sudo aptitude add-user-tag "${TAG}" "${name}"
	fi

}

flush_commands () {
    set +e
    trap sigint_handler SIGINT
	while read -u 11 name command
	do
        res="1"
        BAU="1"
        while [ "${res}" != "0" ] && [ "${BAU}" == "1" ]
        do
            # interrupting the command may move BAU to 1
            BAU="0"
            (
                set -e
                flush_command "${name}" "${command}"
            )
            res="$?"
        done
	done 11<"${LIST_COMMANDS}"
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
