#!/bin/bash -eu

source _ipfs.sh

usage () {
    cat<<EOF
$0

Update the hash
--
A:oldcid:str:The old cid (= to use the finder)
A:newcid:str:The new cid
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

if [ "${CLK___OLDCID}" == "=" ]
then
    oldcid="$(clk ipfs find@sh)"
else
    oldcid="${CLK___OLDCID}"
fi

oldcid="$(ipfs_remove_prefix "${oldcid}")"
newcid="$(ipfs_remove_prefix "${CLK___NEWCID}")"
if ipfs_find_cid "${newcid}"
then
    echo "${newcid} already in the index"
    exit 1
fi
line="$(ipfs_find_cid "${oldcid}")"
echo "${line}"
read -p "Replace this with ${newcid}? (y/n) " answer
if [ "${answer}" == "y" ]
then
    sed -i "s|^/ipfs/${oldcid}|/ipfs/${newcid}|" "${IPFS_INDEX}"
    echo "Removing old cid ${oldcid}"
    ipfs-cluster-ctl pin rm "${oldcid}"
fi
