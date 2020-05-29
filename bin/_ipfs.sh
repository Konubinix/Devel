#!/bin/bash -eu

ipfs_is_key ( ) {
    local key="$1"
    [ "$(echo "${key}"|grep "^Qm"|wc -c)" == "47" ]
}

ipfs_list_ipns_names ( ) {
    ipfs key list -l|cut -f2 -d' '
}

IPFS_INDEX=/home/sam/perso/perso/ipfs_index.txt

ipfs_select_hash ( ) {
    echo "/ipfs/$(cat "${IPFS_INDEX}"|percol|cut -f1 -d' ')"
}

ipfs_key_canonicalize ( ) {
    local key="$1"
    if ! echo "${key}"|grep -q '^/ipfs'
    then
        key="/ipfs/${key}"
    fi
    echo "${key}"
}

ipfs_remove_prefix ( ) {
    local key="$1"
    if echo "${key}"|grep -q '^/ipfs'
    then
        echo "${key}"|sed 's|/ipfs/||'
    else
        echo "${key}"
    fi
}
