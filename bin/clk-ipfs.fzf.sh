#!/bin/bash -eu

source _clk.sh
source _ipfs.sh

usage () {
    cat<<EOF
$0

EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

fzf --with-nth=2.. --bind "enter:execute(mo {1} > /dev/null 2>&1  &)" --multi --history="${TMPDIR}/ipfsfind" --query "${CLK___ARGS}" < "${IPFS_INDEX}"
