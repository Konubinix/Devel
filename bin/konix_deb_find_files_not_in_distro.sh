#!/bin/bash

# TODO, handle the case where several package have the same file, like in
# gnuplot-x11, gnuplot-nox: /usr/bin/gnuplot

ORPHANS="orphans"
PROBLEMS="probs"
SEEN_FILES="seen_files"
SEEN_PACKAGES="seen_packages"
> "$ORPHANS"
> "$SEEN_PACKAGES"
> "$SEEN_FILES"
APT_FILE_REGEXP="^\(.\+, \)\?\([^,]\+\): \(/.\+\)$"
IFS=$'\n'

package () {
	RES="$(echo "$*" | sed "s-${APT_FILE_REGEXP}-\2-")"
}

file () {
	RES="$(echo "$*" | sed "s-${APT_FILE_REGEXP}-\3-")"
}

#apt-file update
find "$1" -type f | while read file
do
	if grep -q "^${file}$" "$SEEN_FILES"
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
 contains more than one line, this may be a bug"|tee "$PROBLEMS">&2
		INFO="$(echo "$INFO"|tail -1)"
		echo "Using only the info $INFO">&2
	fi
	package "$INFO"
	PACKAGE="$RES"
	if grep -q -e "^${PACKAGE}$" "$SEEN_PACKAGES"
	then
		echo "package $PACKAGE already seen, this is a bug">&2
		exit 1
	fi
	# the package has already been dealt with
	echo "$PACKAGE" >> "$SEEN_PACKAGES"
	file "$INFO"
	FILE="$RES"
	# add the files of the package to the seen files
	dpkg -L "${PACKAGE}" >> "$SEEN_FILES"
done
