#!/bin/bash -eu

source _ipfs.sh

usage () {
    cat<<EOF
$0

Check whether the given file is already in ipfs
--
A:file:str:File to check
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi


cid="/ipfs/$(ipfs add --only-hash --quieter "${CLK___FILE}")"
grep "^$cid" "${IPFS_INDEX}"
