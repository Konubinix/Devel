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
F:--gateway:Prepend with the gateway
F:--only-cid:Only the cid, not the full path
F:--with-metadata:Show the metadata as well
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
        fzf --with-nth=2.. --multi --history="${TMPDIR}/ipfsfind" --query "${CLK___ARGS}" < "${IPFS_INDEX}"
    else
        exit 1
    fi
}

gateway () {
    while read cid
    do
        if [ "${CLK___GATEWAY}" == "True" ]
        then
            cid="http://192.168.1.2:9999${cid}"
        fi
        echo "${cid}"
    done
}

only_cid ( ) {
    if [ "${CLK___ONLY_CID}" == "True" ]
    then
        sed 's|^/ipfs/||'
    else
        cat
    fi
}

extractor ( ) {
    if test "${CLK___WITH_METADATA}" == "True"
    then
        cat
    else
        cut -f1 -d' '
    fi
}

cid="$(selector|extractor|gateway|only_cid)"
if [ -z "${cid}" ]
then
    exit 1
else
    echo "${cid}"
fi
