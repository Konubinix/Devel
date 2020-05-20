#!/bin/bash -eu

source _clk.sh
source _ipfs.sh

usage () {
    cat<<EOF
$0

Find a file in ipfs.
--
N:arguments to start search
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

percol --query "${CLK___ARGS}" "${IPFS_INDEX}"|sed -r 's/^([^ ]+) .+/\1/'
