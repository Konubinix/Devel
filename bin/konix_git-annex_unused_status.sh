#!/bin/bash -eu

pushd "$(git rev-parse --show-toplevel)"

usage () {
    cat<<EOF
$0 [-s][-l][-r]

-s: sort
-l: location tracking
-r: restore
EOF
}


SORT=""
LOCATION=""
RESTORE=""
while getopts "hslr" opt; do
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
		r)
			RESTORE=1
			;;
	esac
done

shift $((OPTIND-1))

cut -f2 -d ' ' .git/annex/unused | while read sha
do
	lastname="$(git log -1 --format=oneline --name-status  -S "${sha}"|tail -1|sed 's|^[A-Z]\+[ \t]\+||')"
	lastchanged="$(git annex metadata -g lastchanged --key "${sha}")"
	echo "############### ${lastchanged}: ${lastname} : ${sha}"
	if [ -n "${LOCATION}" ]
	then
		git log --format=oneline --name-status -S "${sha}" 2>&1|cat
		echo
	fi
	if [ -n "${RESTORE}" ]
	then
	   git annex get --key "${sha}"
	   annexobjectlocation="$(git annex contentlocation "${sha}")"
	   dst="RESTORED/${lastname}"
	   mkdir -p "$(dirname "${dst}")"
	   ln -r -s "${annexobjectlocation}" "${dst}"
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
