#!/bin/bash -eu

pushd "$(git rev-parse --show-toplevel)"

usage () {
    cat<<EOF
$0 [-s][-l]

-s: sort
-l: location tracking
EOF
}


SORT=""
LOCATION=""
while getopts "hsl" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		s)
			SORT=1
			;;
		l)
			LOCATION=1
			;;
	esac
done

shift $((OPTIND-1))

cut -f2 -d ' ' .git/annex/unused | while read sha
do
	lastname="$(git log -1 --format=oneline --name-status  -S "${sha}"|tail -1)"
	lastchanged="$(git annex metadata -g lastchanged --key "${sha}")"
	echo "############### ${lastchanged}: ${lastname}"
	if [ -n "${LOCATION}" ]
	then
		git log --format=oneline --name-status  -S "${sha}" 2>&1|cat
		echo
	fi
done | {
	if [ -n "${SORT}" ]
	then
		echo "Sorting. Might take a long time" 2>&1
		sort
	else
		cat
	fi
}

popd
