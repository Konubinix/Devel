#!/bin/bash -eu

source _ipfs.sh

usage () {
    cat<<EOF
$0

Get the metadata, given the key
--
A:key:str:The ipfs key
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

key="${CLK___KEY}"
key="$(ipfs_key_canonicalize "${key}")"
grep "^${key}" "${IPFS_INDEX}"|cut -f 2- -d ' '
