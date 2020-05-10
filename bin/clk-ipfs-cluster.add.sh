#!/bin/bash -eu

source _ipfs-cluster.sh
source _ipfs.sh
source _clk.sh

usage () {
    cat<<EOF
$0

Add a file in ipfs.
--
A:file:str:The file to add in ipfs
O:--name:str:The name to provide in the ipfs-cluster-ctl
O:--ipns-key:$(ipfs key list -l|cut -f 2 -d' '|list_to_choice):A possible key to update also
F:--ipns:Update the ipns of the same key:True
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

if [ "${CLK___IPNS}" == "True" ] && [ "${CLK___IPNS_KEY}" == "" ]
then
    CLK___IPNS_KEY="${CLK___NAME}"
fi

new_cid="$(ipfs-cluster-ctl add --name "${CLK___NAME}" --quiet "${CLK___FILE}"|tail -1)"
if [ "${CLK___IPNS_KEY}" != "" ]
then
    if ! ipfs_list_ipns_names | grep -q "^${CLK___IPNS_KEY}$"
    then
        ipfs key gen --type=rsa --size=2048 "${CLK___IPNS_KEY}"
    fi
    echo "Updating the ipns key ${CLK___IPNS_KEY} with the new cid"
    clk ipfs update-ipns@sh "${CLK___IPNS_KEY}" "${new_cid}"
fi
