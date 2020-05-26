#!/bin/bash -eu

source _clk.sh
source _ipfs.sh
source _ipfs-cluster.sh

usage () {
    cat<<EOF
$0

Add a file in ipfs.
--
A:file:str:The file to add
N:The metadata to add
O:--rmin:int:Minimal number of allocations:-1
O:--rmax:int:Maximal number of allocations:-1
O:-r,--replications:int:Number of replications (0 to remove):2
O:--allocs:$(ipfs_cluster_list_peers|list_to_choice):The allocs
F:--wait/--no-wait:Wait for the file to be correctly pinned everywhere:True
F:-v,--verbose:set -x
F:--path-as-metadata/--no-path-as-metadata:Use the file path as metadata:True
F:--delete:Delete the origin file afterwards
F:--assert-not-present/--no-assert-not-present:Assert that the element is not already in the index:True
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

FILE="$(konix_absolute_path.py "${CLK___FILE}")"

if [ "${CLK___REPLICATIONS}" != "0" ]
then
    CLK___RMIN="${CLK___REPLICATIONS}"
    CLK___RMAX="${CLK___REPLICATIONS}"
fi
if [ "${CLK___VERBOSE}" == "True" ]
then
    set -x
fi

cid="$(ipfs add --quieter --progress --pin=false "$(readlink -f "${FILE}")")"
path="/ipfs/${cid}"
if grep -q "^${path}" "${IPFS_INDEX}" && [ "${CLK___ASSERT_NOT_PRESENT}" == "True" ]
then
    echo "Stopping because ${path} is already in the index"
    exit 1
fi

args=()
if [ "${CLK___ALLOCS}" != "" ]
then
    args+=("--allocs" "${CLK___ALLOCS}")
fi

args+=(--rmin "${CLK___RMIN}" --rmax "${CLK___RMAX}" "${cid}")

echo "Adding ${path} to cluster" >&2
ipfs-cluster-ctl pin add "${args[@]}" >&2

if grep -q "^${path}" "${IPFS_INDEX}"
then
    echo "${path} Already added" >&2
else
    echo "Adding ${path} to the index" >&2
    metadata="added=$(date -Iseconds) "
    if [ "${CLK___PATH_AS_METADATA}" == "True" ]
    then
        metadata+="${FILE} "
    fi
    metadata+="${CLK___ARGS}"
    echo "${path} ${metadata}" >> "${IPFS_INDEX}"
fi

if [ "${CLK___WAIT}" == "True" ]
then
    echo "Waiting for ${path} to be stable" >&2
    ipfs-cluster-ctl pin add --wait "${args[@]}" >&2
fi
if [ "${CLK___DELETE}" == "True" ]
then
    echo "Removing ${FILE}" >&2
    rm "${FILE}"
fi

echo "${cid}"
