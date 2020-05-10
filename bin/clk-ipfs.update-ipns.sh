#!/bin/bash -eu

source _clk.sh
source _ipfs.sh

usage () {
    cat<<EOF
$0

Replace a file in ipfs.
--
A:name:$(ipfs key list -l|cut -f 2 -d' '|list_to_choice):The name of the ipfs key to update
A:file-or-key:str:The file or key to add in ipfs
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

if ipfs_is_key "${CLK___FILE_OR_KEY}"
then
    new_cid="${CLK___FILE_OR_KEY}"
else
    new_cid="$(ipfs add -q "${CLK___FILE_OR_KEY}"|tail -1)"
fi

ipfs name publish --key "${CLK___NAME}" "${new_cid}"
