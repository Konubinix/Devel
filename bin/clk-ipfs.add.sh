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
O:--rmin:int:Minimal number of allocations:-1
O:--rmax:int:Maximal number of allocations:-1
O:--allocs:str:The allocs
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

cid="$(ipfs add --quieter --progress --pin=false "$(readlink -f "${CLK___FILE}")")"
append=1
if grep -q "^$cid" "${IPFS_INDEX}"
then
    echo "${cid} Already added" >&2
    append=0
fi

args=""
if [ "${CLK___ALLOCS}" != "" ]
then
    args+=" --allocs ${CLK___ALLOCS}"
else
    clause
fi


ipfs-cluster-ctl pin add ${args} --rmin "${CLK___RMIN}" --rmax "${CLK___RMAX}" "${cid}" >&2
if [ "${append}" == "1" ]
then
    echo "$cid ${CLK___ARGS}" >> "${IPFS_INDEX}"
fi
echo "${cid}"
