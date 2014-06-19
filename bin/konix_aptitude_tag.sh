#!/bin/bash

set -eu
DATE="$(date +%s)"
DATE_READABLE="$(date -d @${DATE})"
LIST_PACKAGES="$(mktemp)"
LIST_COMMANDS="$(mktemp)"
# echo "LIST_COMMANDS=${LIST_COMMANDS}"
# echo "LIST_PACKAGES=${LIST_PACKAGES}"
trap "rm '${LIST_COMMANDS}' '${LIST_PACKAGES}'" 0

echo "Computing the list of packages"
aptitude search '~i ?not(~M) ?not(?user-tag(.))' > "${LIST_PACKAGES}"
NUMBER="$(wc -l "${LIST_PACKAGES}"|cut -f1 -d' ')"
LAST_RES="d"

flush_commands () {
	while read -u 11 name command
	do
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
	done 11<"${LIST_COMMANDS}"
	echo -n "" > "${LIST_COMMANDS}"
	read -p "Press Enter to continue"
}
trap "rm '${LIST_COMMANDS}' '${LIST_PACKAGES}'" 0

while read -u 10 line
do
    NAME="$(echo "$line" | sed -r 's/^[^ ]+ +([^ ]+).+$/\1/')"
	clear
    echo "--------------- ${NUMBER} left"
	NUMBER=$((NUMBER-1))
    aptitude show "${NAME}"
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
flush_commands
