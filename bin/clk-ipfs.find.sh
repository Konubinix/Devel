#!/bin/bash -eu

source _clk.sh
source _ipfs.sh

usage () {
    cat<<EOF
$0

Find a file in ipfs.
--
N:arguments to start search
O:--selector:["dmenu", "fzf", "percol"]:The selector to use:fzf
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

selector ( ) {
    if [ "${CLK___SELECTOR}" == "dmenu" ]
    then
        konix_dmenu_custom.sh < "${IPFS_INDEX}"
    elif [ "${CLK___SELECTOR}" == "percol" ]
    then
        percol --query "${CLK___ARGS}" "${IPFS_INDEX}"
    elif [ "${CLK___SELECTOR}" == "fzf" ]
    then
        fzf --with-nth=2.. --query "${CLK___ARGS}" < "${IPFS_INDEX}"
    else
        exit 1
    fi
}

selector|sed -r 's|^([^ ]+) .+|\1|'|tr -d '\n'
