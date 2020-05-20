#!/bin/bash -eu

source _ipfs-cluster.sh
source _clk.sh

usage () {
    cat<<EOF
$0

Replace a file in ipfs.
--
A:name:$(ipfs_cluster_pins_names|list_to_choice):The name of the ipfs-cluster-ctl pin
A:file:str:The file to add in ipfs
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

old_cid="$(ipfs_cluster_name_to_pin "${CLK___NAME}")"
new_cid="$(ipfs add -q "${CLK___FILE}"|tail -1)"
if [ "${old_cid}" == "${new_cid}" ]
then
    echo "Pin is already up-to-date"
else
    echo "Updating the pin"
    ipfs-cluster-ctl pin update "${old_cid}" "${new_cid}"
    echo "Removing the old pin"
    ipfs-cluster-ctl pin rm "${old_cid}"
fi
if [ "${CLK___IPNS_KEY}" != "" ]
then
    echo "Updating the ipns key ${CLK___IPNS_KEY} with the new cid"
    clk ipfs update-ipns@sh "${CLK___IPNS_KEY}" "${new_cid}"
fi
