#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Show the cumulative size of several ipfs objects
--
N:ipfs hashs
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0

totalsize=0
n=0
for cid in ${CLK___ARGS}
do
    n=$((n+1))
    echo "${cid}"
    ipfs object stat --human "${cid}" | tee "${TMPDIR}/size"
    si_size="$(cat "${TMPDIR}/size"\
|grep CumulativeSize\
|cut -f 2- -d ' '\
|sed 's/B//'|sed 's/ //'|sed 's/k/K/'\
)"
    size="$(numfmt --from si --to none "${si_size}")"
    totalsize="$(echo "${totalsize} + ${size}"|bc -l)"
    echo "${n}, TotalSize: $(numfmt --to=si --suffix=B --padding=7 "${totalsize}")"
done
