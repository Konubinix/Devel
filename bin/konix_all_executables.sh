#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

IFS=":" read -ra DIRECTORIES <<< ${PATH}

EXISTING_DIRECTORIES=()
for directory in "${DIRECTORIES[@]}"
do
	if [ -e "${directory}" ]
	then
		EXISTING_DIRECTORIES+=("${directory}")
	fi
done

find "${EXISTING_DIRECTORIES[@]}" -maxdepth 1 -type f -executable
