#!/bin/bash -eu

source _ipfs.sh
source _clk.sh

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

ipfs_select_hash
