#!/bin/bash -eux

pushd "$(git rev-parse --show-toplevel)"

usage () {
    cat<<EOF
$0 [-s][-l][-r]

-s: sort
-c: compute them before doing the other stuff
-l: location tracking
-r: restore
EOF
}


SORT=""
LOCATION=""
RESTORE=""
while getopts "hslrc" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		s)
			SORT=1
			;;
		c)
			git annex unused
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
	ack="$(git annex metadata -g ack --key "${sha}")"
	if [ "${ack}" == "delete" ]
	then
		git annex drop --force --key "${sha}"
		continue
	fi
	# git log return an expression to be evaluated 3 times before giving the correct string, hence the two nested printf
	lastname="$(printf "$(printf "$(git log -1 --format='%e %s' --name-status -S "${sha}" | tail -1|sed 's|^[A-Z]\+[ \t]\+||'|sed 's|^"\\"||'|sed 's|\\""$||')")"|sed 's|^"||'|sed 's|"$||')"
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
	   dst="${lastname}"
	   if [ "${dst}" == "" ]
	   then
		   dst="noname"
	   fi
	   while [ -e "$dst" ] || [ -L "$dst" ]
	   do
		   dst="$(mktemp -u "${dst}.XXXX")"
	   done
	   DIR="$(dirname "${dst}")"
	   if [ "${DIR}" != "" ]
	   then
		   mkdir -p "$(dirname "${dst}")"
	   fi
	   ln -v -r -s "${annexobjectlocation}" "${dst}"
	   git annex metadata --set ack=restored --key="${sha}"
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
