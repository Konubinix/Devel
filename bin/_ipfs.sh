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
