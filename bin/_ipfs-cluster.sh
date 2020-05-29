#!/bin/bash -eu

source _clk.sh

IPFS_CLUSTER_FILTER_OPTION='O:--filter:["cluster_error", "error", "pin_error", "pin_queued", "pinned", "pinning", "queued", "remote", "unpin_error", "unpin_queued", "unpinned", "unpinning"]: Filter to use'

ipfs_cluster_pins_names () {
    ipfs-cluster-ctl pin ls|cut -f2 -d'|'|awk '{$1=$1};1'
}

ipfs_cluster_name_to_pin () {
    local name="$1"
    ipfs-cluster-ctl pin ls|grep "| ${name} |"|cut -f1 -d'|'|trim
}

ipfs_cluster_list_peers ( ) {
    ipfs-cluster-ctl peers ls |grep '^12D'|cut -f 1 -d' '
}
