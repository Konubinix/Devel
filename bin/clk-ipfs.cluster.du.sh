#!/bin/bash -eu

source _ipfs-cluster.sh

usage () {
    cat<<EOF
$0

Show the size of the matching refs
--
${IPFS_CLUSTER_FILTER_OPTION}
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

args=()
if [ "${CLK___FILTER}" != "" ]
then
    args+=("--filter" "${CLK___FILTER}")
fi

clk ipfs du@sh $(ipfs-cluster-ctl --enc=json status "${args[@]}"|jq -r '.[].cid."/"')
