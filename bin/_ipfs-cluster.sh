#!/bin/bash -eu

source _clk.sh

ipfs_cluster_pins_names () {
    ipfs-cluster-ctl pin ls|cut -f2 -d'|'|awk '{$1=$1};1'
}

ipfs_cluster_name_to_pin () {
    local name="$1"
    ipfs-cluster-ctl pin ls|grep "| ${name} |"|cut -f1 -d'|'|trim
}
