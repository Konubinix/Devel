#!/bin/bash

ORPHANS="orphans"
PROBLEMS="probs"
SEEN_FILES="seen_files"
SEEN_PACKAGES="seen_packages"

usage () {
	cat<<EOF
$0 [-h|-c] -d <directory>
-h show this help and exits
-c continue: do not erase the results files and use them
-d where to look for files
EOF
}
CONTINUE=0
DIRECTORY=""
while getopts "hcd:" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		c)
			CONTINUE=1
			;;
		d)
			DIRECTORY="$OPTARG"
			;;
	esac
done

if [ -z "$DIRECTORY" ]
then
	usage
	exit 1
fi

if [ "$CONTINUE" == "0" ]
then
	> "$ORPHANS"
	> "$SEEN_PACKAGES"
	> "$SEEN_FILES"
fi

APT_FILE_REGEXP="^\(.\+\): \(/.\+\)$"
IFS=$'\n'

packages () {
	RES="$(echo "$*" | sed "s-${APT_FILE_REGEXP}-\1-"|sed "s/, /\n/")"
}

file () {
	RES="$(echo "$*" | sed "s-${APT_FILE_REGEXP}-\2-")"
}

encode () {
	echo "$*" | base64
}

decode () {
	echo "$*" | base64 -d
}

#apt-file update
find "$DIRECTORY" -type f | while read file
do
	ENCODED_FILE=$(encode "${file}")
	if grep -q "^${ENCODED_FILE}$" "$SEEN_FILES"
	then
		# already dealt with this file
		continue
	fi
	# the file should belong to a package
	INFO="$(dpkg -S "$file")"
	if [ "$?" != "0" ]
	then
		echo "$file ORPHAN !!"
		echo "$file" >> "$ORPHANS"
		continue
	fi
	echo "${INFO}" >&2
	_nb_lines="$(echo "$INFO" |wc -l)"
	if ! echo "${_nb_lines}"|grep -q "^1$"
	then
		echo "$INFO, got with dpkg -S $file,\
 contains more than one line, this may be a bug"|tee -a "$PROBLEMS">&2
		INFO="$(echo "$INFO"|tail -1)"
		echo "Using only the info $INFO">&2
	fi
	packages "$INFO"
	for PACKAGE in $RES
	do
		ENCODED_PACKAGE="$(encode "$PACKAGE")"
		if grep -q -e "^${ENCODED_PACKAGE}$" "$SEEN_PACKAGES"
		then
			echo "package $PACKAGE (${ENCODED_PACKAGE}) already seen, this is a bug">&2
			exit 1
		fi
     	# the package has already been dealt with
		echo "$ENCODED_PACKAGE" >> "$SEEN_PACKAGES"
    	# add the files of the package to the seen files
		PACKAGE_FILES="$(dpkg -L "${PACKAGE}")"
		for package_file in ${PACKAGE_FILES}
		do
			encode "${package_file}" >> "$SEEN_FILES"
		done
	done
done
