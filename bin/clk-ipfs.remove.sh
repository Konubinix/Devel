#!/bin/bash -eu

source _ipfs.sh

usage () {
    cat<<EOF
$0

Remove this cid from the index
--
A:cid:str:The cid to take into account (= to use the finder)
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

if [ "${CLK___CID}" == "=" ]
then
    cid="$(clk ipfs find@sh)"
else
    cid="${CLK___CID}"
fi

cid="$(ipfs_remove_prefix "${cid}")"
line="$(ipfs_find_cid "${cid}")"
echo "${line}"
read -p "ok? (y/n) " answer
if [ "${answer}" == "y" ]
then
    sed -i "/^\/ipfs\/${cid}/d" "${IPFS_INDEX}"
    echo "Removing from the cluster ${cid}"
    ipfs-cluster-ctl pin rm "${cid}"
fi
