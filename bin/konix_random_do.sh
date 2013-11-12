#!/bin/bash

usage () {
	cat<<EOF
$0 [-h] [-l <ls_options>]> -d <do> -D <directory_do>
EOF
}

while getopts "hl:d:D:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            LS_OPTIONS="${OPTARG}"
            ;;
        d)
            DO="${OPTARG}"
            ;;
        D)
            DIRECTORY_DO="${OPTARG}"
            ;;
    esac
done

if [ "${DO}" == "" ]
then
	echo "You must provide something to do"
	usage
	exit 1
fi

FILE=`ls $LS_OPTIONS |konix_random_choose.sh`
echo "Acting on file '${FILE}'"
if [ -n "${DIRECTORY_DO}" ] && [ -d "${FILE}" ]
then
	DO="$DIRECTORY_DO"
fi

$DO "${FILE}"
