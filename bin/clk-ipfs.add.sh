#!/bin/bash -eu

source _clk.sh
source _ipfs.sh

usage () {
    cat<<EOF
$0

Add a file in ipfs.
--
A:file:str:The file to add
N:The metadata to add
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

cid="$(ipfs add --quieter --progress --pin=false "$(readlink -f "${CLK___FILE}")")"
if grep -q "^$cid" "${IPFS_INDEX}"
then
    echo "${cid} Already added"
    exit 1
fi

ipfs-cluster-ctl pin add "${cid}"

echo "$cid ${*}" >> "${IPFS_INDEX}"
