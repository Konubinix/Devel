#!/bin/bash -eu

usage () {
    cat<<EOF
$0

--
N:Args to give to recoll
O:-s,--state:str:State to use:done
O:-m,--method:str:Method to use:find
N:Remaining args
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

konix_recollq.py -b "$@" | \
    sed 's|file://||' | \
    konix_lines2args.py git annex "${CLK___METHOD}" --metadata state="${CLK___STATE}" "${@}"
