#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Open an ipfs file
--
O:--selector:["dmenu", "fzf", "percol"]:The selector to use:fzf
N:Start search
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

while read cid
do
    echo "Opening ${cid}"
    mimeopen "${cid}"
done < <(clk ipfs find@sh --selector "${CLK___SELECTOR}" ${CLK___ARGS})
