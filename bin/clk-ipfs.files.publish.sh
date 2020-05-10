#!/bin/bash -eu

source _ipfs.sh
source _clk.sh

usage () {
    cat<<EOF
$0

Publish a MFS file.
--
A:name:$(ipfs_list_ipns_names|list_to_choice):The name of the ipfs key to update
A:file:str:The file to publish
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

ipfs name publish --key "${CLK___NAME}" "$(ipfs files stat --hash "${CLK___FILE}")"
